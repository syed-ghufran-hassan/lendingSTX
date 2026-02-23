;; lending-pool-v1.clar
;; Advanced Lending Pool with Dynamic Interest Rates, Liquidation Logic, and Per-User Borrow Limits

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-not-found (err u100))
(define-constant err-insufficient-balance (err u101))
(define-constant err-health-factor (err u102))
(define-constant err-unauthorized (err u103))
(define-constant err-liquidation (err u104))
(define-constant err-math (err u105))
(define-constant err-slippage (err u106))

;; Interest rate constants (using WadRay math - 1e27 precision)
(define-constant RAY u1000000000000000000000000000) ;; 1e27
(define-constant HALF_RAY u500000000000000000000000000) ;; 0.5e27
(define-constant PERCENTAGE_FACTOR u10000) ;; 100% = 10000 basis points

;; Interest rate model parameters
(define-constant OPTIMAL_UTILIZATION_RATE u8000) ;; 80%
(define-constant BASE_BORROW_RATE u100) ;; 1%
(define-constant SLOPE1 u400) ;; 4% (up to optimal)
(define-constant SLOPE2 u1000) ;; 10% (above optimal)

;; Liquidation parameters
(define-constant LIQUIDATION_THRESHOLD u8000) ;; 80% LTV
(define-constant LIQUIDATION_BONUS u10500) ;; 5% bonus
(define-constant LIQUIDATION_CLOSE_FACTOR u5000) ;; 50% of debt

;; Data Maps
(define-map reserves
    { token: principal }
    {
        name: (string-ascii 32),
        symbol: (string-ascii 10),
        decimals: uint,
        total-liquidity: uint,
        total-borrows: uint,
        borrow-index: uint,
        liquidity-index: uint,
        last-update-block: uint,
        interest-rate: uint,
        available-liquidity: uint,
        is-active: bool
    }
)

(define-map user-supplies
    { user: principal, token: principal }
    { amount: uint, scaled-balance: uint }
)

(define-map user-borrows
    { user: principal, token: principal }
    { amount: uint, scaled-amount: uint, interest-start: uint }
)

(define-map user-borrow-limits
    { user: principal, token: principal }
    { max-borrow: uint }
)

(define-map user-collateral-status
    { user: principal, token: principal }
    { enabled: bool }
)

(define-map token-prices
    { token: principal }
    { price: uint, decimals: uint, last-update: uint }
)

(define-data-var reserves-count uint u0)
(define-map reserve-list { index: uint } principal)

;; Borrow limit management (owner-only)
(define-public (set-user-borrow-limit (user principal) (token principal) (max-borrow uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
        (map-set user-borrow-limits { user: user, token: token } { max-borrow: max-borrow })
        (ok true)
    )
)

;; Borrow tokens from the pool (enforcing per-user borrow limit)
(define-public (borrow (token principal) (amount uint))
    (let (
        (reserve (unwrap! (map-get? reserves { token: token }) err-not-found))
        (sender tx-sender)
        (collateral-value (get-collateral-value sender))
        (debt-value (get-debt-value sender))
        (health-factor (calculate-health-factor collateral-value debt-value))
        (user-borrow (default-to { amount: u0, scaled-amount: u0, interest-start: burn-block-height } 
                      (map-get? user-borrows { user: sender, token: token })))
        (user-limit (default-to u0 (get max-borrow (map-get? user-borrow-limits { user: sender, token: token }))))
    )
        (begin
            (asserts! (get is-active reserve) err-not-found)
            (asserts! (>= health-factor RAY) err-health-factor)
            (asserts! (>= (get available-liquidity reserve) amount) err-insufficient-balance)
            
            ;; Enforce per-user borrow limit
            (asserts! (<= (+ (get amount user-borrow) amount) user-limit) err-insufficient-balance)
            
            ;; Update indexes
            (try! (update-indexes token))
            
            ;; Continue with borrow logic
            (let (
                (updated-reserve (unwrap! (map-get? reserves { token: token }) err-not-found))
                (borrow-index (get borrow-index updated-reserve))
                (new-scaled (+ (get scaled-amount user-borrow) (/ (* amount RAY) borrow-index)))
                (new-amount (+ (get amount user-borrow) amount))
            )
                (begin
                    (map-set user-borrows { user: sender, token: token }
                        { amount: new-amount, scaled-amount: new-scaled, interest-start: burn-block-height })
                    
                    (map-set reserves { token: token }
                        (merge updated-reserve {
                            total-borrows: (+ (get total-borrows updated-reserve) amount),
                            available-liquidity: (- (get available-liquidity updated-reserve) amount),
                            interest-rate: (calculate-borrow-rate 
                                (/ (* (+ (get total-borrows updated-reserve) amount) u10000)
                                   (get total-liquidity updated-reserve)))
                        }))
                    
                    (print { event: "borrow", user: sender, token: token, amount: amount })
                    (ok true)
                )
            )
        )
    )
)
