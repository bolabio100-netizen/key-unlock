;; Key Unlock Protocol - Simplified Core Contract
;; A DeFi protocol for perpetual futures trading with NFT key-based position management

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-insufficient-balance (err u103))
(define-constant err-unauthorized (err u104))
(define-constant err-invalid-amount (err u105))
(define-constant err-position-not-found (err u106))

;; Data Variables
(define-data-var next-key-id uint u1)
(define-data-var next-position-id uint u1)
(define-data-var protocol-fee-rate uint u30) ;; 0.3% (30 basis points)
(define-data-var total-protocol-revenue uint u0)

;; NFT Key Definition
(define-non-fungible-token trading-key uint)

;; Data Maps
(define-map key-metadata
    uint
    {
        owner: principal,
        tier: (string-ascii 20),
        trading-volume: uint,
        profit-loss: int,
        fee-discount: uint,
        max-leverage: uint,
        created-at: uint
    }
)

(define-map trading-positions
    uint
    {
        trader: principal,
        key-id: uint,
        asset: (string-ascii 10),
        size: uint,
        leverage: uint,
        entry-price: uint,
        collateral: uint,
        is-long: bool,
        is-active: bool,
        created-at: uint
    }
)

(define-map user-collateral
    principal
    uint
)

(define-map liquidity-pools
    (string-ascii 10)
    {
        total-liquidity: uint,
        available-liquidity: uint,
        utilization-rate: uint
    }
)

;; Read-Only Functions
(define-read-only (get-key-metadata (key-id uint))
    (map-get? key-metadata key-id)
)

(define-read-only (get-position (position-id uint))
    (map-get? trading-positions position-id)
)

(define-read-only (get-user-collateral (user principal))
    (default-to u0 (map-get? user-collateral user))
)

(define-read-only (get-liquidity-pool (asset (string-ascii 10)))
    (map-get? liquidity-pools asset)
)

(define-read-only (get-protocol-fee-rate)
    (ok (var-get protocol-fee-rate))
)

(define-read-only (calculate-position-fee (size uint) (key-id uint))
    (let
        (
            (base-fee (var-get protocol-fee-rate))
            (key-data (unwrap! (map-get? key-metadata key-id) (err u0)))
            (discount (get fee-discount key-data))
            (discounted-fee (- base-fee discount))
        )
        (ok (/ (* size discounted-fee) u10000))
    )
)

;; Public Functions

;; Mint a new trading key NFT
(define-public (mint-trading-key)
    (let
        (
            (key-id (var-get next-key-id))
        )
        (try! (nft-mint? trading-key key-id tx-sender))
        (map-set key-metadata key-id
            {
                owner: tx-sender,
                tier: "BRONZE",
                trading-volume: u0,
                profit-loss: 0,
                fee-discount: u0,
                max-leverage: u10,
                created-at: block-height
            }
        )
        (var-set next-key-id (+ key-id u1))
        (ok key-id)
    )
)

;; Deposit collateral
(define-public (deposit-collateral (amount uint))
    (begin
        (asserts! (> amount u0) err-invalid-amount)
        (let
            (
                (current-balance (get-user-collateral tx-sender))
            )
            (map-set user-collateral tx-sender (+ current-balance amount))
            (ok true)
        )
    )
)

;; Withdraw collateral
(define-public (withdraw-collateral (amount uint))
    (let
        (
            (current-balance (get-user-collateral tx-sender))
        )
        (asserts! (>= current-balance amount) err-insufficient-balance)
        (map-set user-collateral tx-sender (- current-balance amount))
        (ok true)
    )
)

;; Open a trading position
(define-public (open-position 
    (key-id uint)
    (asset (string-ascii 10))
    (size uint)
    (leverage uint)
    (entry-price uint)
    (collateral-amount uint)
    (is-long bool))
    (let
        (
            (position-id (var-get next-position-id))
            (key-data (unwrap! (map-get? key-metadata key-id) err-not-found))
            (user-balance (get-user-collateral tx-sender))
        )
        ;; Verify key ownership
        (asserts! (is-eq (unwrap! (nft-get-owner? trading-key key-id) err-not-found) tx-sender) err-unauthorized)
        
        ;; Check leverage limit
        (asserts! (<= leverage (get max-leverage key-data)) err-unauthorized)
        
        ;; Check collateral
        (asserts! (>= user-balance collateral-amount) err-insufficient-balance)
        
        ;; Deduct collateral
        (map-set user-collateral tx-sender (- user-balance collateral-amount))
        
        ;; Create position
        (map-set trading-positions position-id
            {
                trader: tx-sender,
                key-id: key-id,
                asset: asset,
                size: size,
                leverage: leverage,
                entry-price: entry-price,
                collateral: collateral-amount,
                is-long: is-long,
                is-active: true,
                created-at: block-height
            }
        )
        
        ;; Update key metadata
        (map-set key-metadata key-id
            (merge key-data { trading-volume: (+ (get trading-volume key-data) size) })
        )
        
        (var-set next-position-id (+ position-id u1))
        (ok position-id)
    )
)

;; Close a trading position
(define-public (close-position (position-id uint) (exit-price uint))
    (let
        (
            (position (unwrap! (map-get? trading-positions position-id) err-position-not-found))
        )
        ;; Verify ownership
        (asserts! (is-eq (get trader position) tx-sender) err-unauthorized)
        (asserts! (get is-active position) err-position-not-found)
        
        ;; Mark position as closed
        (map-set trading-positions position-id
            (merge position { is-active: false })
        )
        
        ;; Return collateral (simplified - actual would calculate PnL)
        (let
            (
                (current-balance (get-user-collateral tx-sender))
                (returned-amount (get collateral position))
            )
            (map-set user-collateral tx-sender (+ current-balance returned-amount))
        )
        
        (ok true)
    )
)

;; Upgrade key tier based on performance
(define-public (upgrade-key-tier (key-id uint))
    (let
        (
            (key-data (unwrap! (map-get? key-metadata key-id) err-not-found))
        )
        (asserts! (is-eq (get owner key-data) tx-sender) err-unauthorized)
        
        ;; Simple tier upgrade logic based on trading volume
        (let
            (
                (volume (get trading-volume key-data))
                (new-tier 
                    (if (>= volume u1000000)
                        "GOLD"
                        (if (>= volume u500000)
                            "SILVER"
                            "BRONZE"
                        )
                    )
                )
                (new-discount
                    (if (is-eq new-tier "GOLD")
                        u15
                        (if (is-eq new-tier "SILVER")
                            u10
                            u0
                        )
                    )
                )
                (new-leverage
                    (if (is-eq new-tier "GOLD")
                        u50
                        (if (is-eq new-tier "SILVER")
                            u25
                            u10
                        )
                    )
                )
            )
            (map-set key-metadata key-id
                (merge key-data 
                    {
                        tier: new-tier,
                        fee-discount: new-discount,
                        max-leverage: new-leverage
                    }
                )
            )
            (ok new-tier)
        )
    )
)

;; Add liquidity to a pool
(define-public (add-liquidity (asset (string-ascii 10)) (amount uint))
    (begin
        (asserts! (> amount u0) err-invalid-amount)
        (let
            (
                (pool (default-to 
                    { total-liquidity: u0, available-liquidity: u0, utilization-rate: u0 }
                    (map-get? liquidity-pools asset)
                ))
            )
            (map-set liquidity-pools asset
                {
                    total-liquidity: (+ (get total-liquidity pool) amount),
                    available-liquidity: (+ (get available-liquidity pool) amount),
                    utilization-rate: (get utilization-rate pool)
                }
            )
            (ok true)
        )
    )
)

;; Admin Functions

;; Update protocol fee rate
(define-public (set-protocol-fee-rate (new-rate uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (<= new-rate u100) err-invalid-amount) ;; Max 1%
        (var-set protocol-fee-rate new-rate)
        (ok true)
    )
)

;; Initialize contract
(begin
    (map-set liquidity-pools "BTC"
        { total-liquidity: u0, available-liquidity: u0, utilization-rate: u0 }
    )
    (map-set liquidity-pools "ETH"
        { total-liquidity: u0, available-liquidity: u0, utilization-rate: u0 }
    )
    (map-set liquidity-pools "STX"
        { total-liquidity: u0, available-liquidity: u0, utilization-rate: u0 }
    )
)