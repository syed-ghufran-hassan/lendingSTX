;; lending-pool-v1.clar
;; Advanced Lending Pool with Dynamic Interest Rates and Liquidation Logic

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
        total-liquidity: uint,      ;; Total supplied
        total-borrows: uint,        ;; Total borrowed
        borrow-index: uint,          ;; Cumulative borrow index (RAY)
        liquidity-index: uint,        ;; Cumulative supply index (RAY)
        last-update-block: uint,
        interest-rate: uint,          ;; Current borrow rate (basis points)
        available-liquidity: uint,     ;; Actual token balance
        is-active: bool
    }
)

(define-map user-supplies
    { user: principal, token: principal }
    { amount: uint, scaled-balance: uint }  ;; scaled by index
)

(define-map user-borrows
    { user: principal, token: principal }
    { amount: uint, scaled-amount: uint, interest-start: uint }
)

(define-map user-collateral-status
    { user: principal, token: principal }
    { enabled: bool }
)

(define-map token-prices
    { token: principal }
    { price: uint, decimals: uint, last-update: uint }  ;; price in USD with 8 decimals
)

(define-data-var reserves-count uint u0)
(define-map reserve-list { index: uint } principal)

;; Helper Functions - Math with RAY precision
(define-private (ray-mul (a uint) (b uint))
    (/ (* a b) RAY)
)

(define-private (ray-div (a uint) (b uint))
    (/ (* a RAY) b)
)

(define-private (wad-to-ray (wad uint))
    (* wad u1000000000)  ;; 1e9
)

(define-private (ray-to-wad (ray uint))
    (/ ray u1000000000)
)

(define-private (min (a uint) (b uint))
    (if (< a b) a b)
)

(define-private (max (a uint) (b uint))
    (if (> a b) a b)
)

;; Interest Rate Calculation
;; Based on utilization rate: U = totalBorrows / totalLiquidity
(define-private (calculate-borrow-rate (utilization uint))
    (let (
        (util (if (< utilization u10000) utilization u10000))  ;; Cap at 100% (replacing min)
        )
        (if (<= util OPTIMAL_UTILIZATION_RATE)
            ;; Below optimal: Base rate + (util/optimal) * slope1
            (+ BASE_BORROW_RATE 
               (/ (* util SLOPE1) OPTIMAL_UTILIZATION_RATE))
            ;; Above optimal: Base + slope1 + ((util-optimal)/(100-optimal)) * slope2
            (+ BASE_BORROW_RATE SLOPE1
               (/ (* (- util OPTIMAL_UTILIZATION_RATE) SLOPE2) 
                  (- u10000 OPTIMAL_UTILIZATION_RATE))
            )
        )
    )
)

;; Index Update Function
(define-private (update-indexes (token principal))
    (let (
        (reserve (unwrap! (map-get? reserves { token: token }) err-not-found))
        (current-block burn-block-height)
        (last-update (get last-update-block reserve))
        (time-delta (- current-block last-update))
        (borrow-rate (get interest-rate reserve))
        )
        (if (> time-delta u0)
            (let (
                ;; Calculate new borrow index
                (rate-per-block (/ (* borrow-rate RAY) (* u10000 u5256000))) ;; Approx blocks per year
                (borrow-accum (ray-mul (get borrow-index reserve) 
                                       (+ RAY (ray-mul rate-per-block time-delta))))
                
                ;; Calculate new liquidity index (supply index grows slower due to spread)
                ;; Assuming 10% spread between borrow and supply
                (supply-rate (/ (* borrow-rate u9000) u10000)) ;; 90% of borrow rate
                (supply-rate-per-block (/ (* supply-rate RAY) (* u10000 u5256000)))
                (liquidity-accum (ray-mul (get liquidity-index reserve)
                                          (+ RAY (ray-mul supply-rate-per-block time-delta))))
                )
                (begin
                    (map-set reserves { token: token }
                        (merge reserve {
                            borrow-index: borrow-accum,
                            liquidity-index: liquidity-accum,
                            last-update-block: current-block
                        }))
                    (ok true)
                )
            )
            (ok false)  ;; No update needed
        )
    )
)

;; Initialize a new reserve
(define-public (init-reserve
    (token principal)
    (name (string-ascii 32))
    (symbol (string-ascii 10))
    (decimals uint)
    (price uint)  ;; Initial price in USD with 8 decimals
    )
    (let ((reserve-id (var-get reserves-count)))
        (begin
            (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
            (asserts! (is-none (map-get? reserves { token: token })) err-not-found)
            
            (map-set reserves { token: token }
                {
                    name: name,
                    symbol: symbol,
                    decimals: decimals,
                    total-liquidity: u0,
                    total-borrows: u0,
                    borrow-index: RAY,
                    liquidity-index: RAY,
                    last-update-block: burn-block-height,
                    interest-rate: BASE_BORROW_RATE,
                    available-liquidity: u0,
                    is-active: true
                })
            
            (map-set token-prices { token: token }
                {
                    price: price,
                    decimals: u8,
                    last-update: burn-block-height
                })
            
            (map-set reserve-list { index: reserve-id } token)
            (var-set reserves-count (+ reserve-id u1))
            (ok true)
        )
    )
)

;; Supply tokens to the pool
(define-public (supply (token principal) (amount uint))
    (let (
        (reserve (unwrap! (map-get? reserves { token: token }) err-not-found))
        (sender tx-sender)
        )
        (begin
            (asserts! (get is-active reserve) err-not-found)
            
            ;; Update indexes first
            (try! (update-indexes token))
            
            ;; Update reserve with new indexes
            (let (
                (updated-reserve (unwrap! (map-get? reserves { token: token }) err-not-found))
                (user-supply (default-to { amount: u0, scaled-balance: u0 } 
                    (map-get? user-supplies { user: sender, token: token })))
                (liquidity-index (get liquidity-index updated-reserve))
                )
                (begin
                    ;; Calculate scaled balance for future interest accrual
                    (let (
                        (new-scaled (+ (get scaled-balance user-supply) 
                                      (/ (* amount RAY) liquidity-index)))
                        (new-amount (+ (get amount user-supply) amount))
                        )
                        (begin
                            (map-set user-supplies { user: sender, token: token }
                                {
                                    amount: new-amount,
                                    scaled-balance: new-scaled
                                })
                            
                            ;; Update reserve totals
                            (map-set reserves { token: token }
                                (merge updated-reserve {
                                    total-liquidity: (+ (get total-liquidity updated-reserve) amount),
                                    available-liquidity: (+ (get available-liquidity updated-reserve) amount)
                                }))
                            
                            ;; Emit event
                            (print { event: "supply", user: sender, token: token, amount: amount })
                            (ok true)
                        )
                    )
                )
            )
        )
    )
)

;; Borrow tokens from the pool
(define-public (borrow (token principal) (amount uint))
    (let (
        (reserve (unwrap! (map-get? reserves { token: token }) err-not-found))
        (sender tx-sender)
        (collateral-value (get-collateral-value sender))
        (debt-value (get-debt-value sender))
        (health-factor (calculate-health-factor collateral-value debt-value))
        )
        (begin
            (asserts! (get is-active reserve) err-not-found)
            (asserts! (>= health-factor RAY) err-health-factor)  ;; Health factor must be >= 1
            
            ;; Check utilization and liquidity
            (asserts! (>= (get available-liquidity reserve) amount) err-insufficient-balance)
            
            ;; Update indexes
            (try! (update-indexes token))
            
            (let (
                (updated-reserve (unwrap! (map-get? reserves { token: token }) err-not-found))
                (user-borrow (default-to { amount: u0, scaled-amount: u0, interest-start: burn-block-height }
                    (map-get? user-borrows { user: sender, token: token })))
                (borrow-index (get borrow-index updated-reserve))
                )
                (begin
                    ;; Calculate scaled borrow amount
                    (let (
                        (new-scaled (+ (get scaled-amount user-borrow) 
                                      (/ (* amount RAY) borrow-index)))
                        (new-amount (+ (get amount user-borrow) amount))
                        )
                        (begin
                            (map-set user-borrows { user: sender, token: token }
                                {
                                    amount: new-amount,
                                    scaled-amount: new-scaled,
                                    interest-start: burn-block-height
                                })
                            
                            ;; Update reserve
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
    )
)

;; Repay borrowed tokens
(define-public (repay (token principal) (amount uint))
    (let (
        (reserve (unwrap! (map-get? reserves { token: token }) err-not-found))
        (sender tx-sender)
        (user-borrow (unwrap! (map-get? user-borrows { user: sender, token: token }) err-not-found))
        )
        (begin
            (asserts! (get is-active reserve) err-not-found)
            
            ;; Update indexes
            (try! (update-indexes token))
            
            (let (
                (updated-reserve (unwrap! (map-get? reserves { token: token }) err-not-found))
                (borrow-index (get borrow-index updated-reserve))
                (current-debt (get amount user-borrow))
                (repay-amount (if (< amount current-debt) amount current-debt))  ;; min replacement
                )
                (begin
                    ;; Calculate accrued interest
                    (let (
                        (scaled-debt (get scaled-amount user-borrow))
                        (principal-with-interest (/ (* scaled-debt borrow-index) RAY))
                        (interest (- principal-with-interest current-debt))
                        )
                        (begin
                            ;; Update user borrow
                            (let (
                                (new-amount (- current-debt repay-amount))
                                (new-scaled (if (> new-amount u0)
                                              (/ (* new-amount RAY) borrow-index)
                                              u0))
                                )
                                (if (> new-amount u0)
                                    (map-set user-borrows { user: sender, token: token }
                                        {
                                            amount: new-amount,
                                            scaled-amount: new-scaled,
                                            interest-start: burn-block-height
                                        })
                                    (map-delete user-borrows { user: sender, token: token })
                                )
                            )
                            
                            ;; Update reserve
                            (map-set reserves { token: token }
                                (merge updated-reserve {
                                    total-borrows: (- (get total-borrows updated-reserve) repay-amount),
                                    available-liquidity: (+ (get available-liquidity updated-reserve) repay-amount),
                                    interest-rate: (calculate-borrow-rate 
                                        (/ (* (- (get total-borrows updated-reserve) repay-amount) u10000)
                                           (get total-liquidity updated-reserve)))
                                }))
                            
                            (print { event: "repay", user: sender, token: token, amount: repay-amount, interest: interest })
                            (ok true)
                        )
                    )
                )
            )
        )
    )
)

;; Liquidate an unhealthy position
(define-public (liquidate
    (user principal)
    (debt-token principal)
    (collateral-token principal)
    (debt-amount uint)
    )
    (let (
        (debt-reserve (unwrap! (map-get? reserves { token: debt-token }) err-not-found))
        (collateral-reserve (unwrap! (map-get? reserves { token: collateral-token }) err-not-found))
        (liquidator tx-sender)
        (user-debt (unwrap! (map-get? user-borrows { user: user, token: debt-token }) err-not-found))
        (user-collateral-amount (unwrap! (map-get? user-supplies { user: user, token: collateral-token }) err-not-found))
        )
        (begin
            (asserts! (get is-active debt-reserve) err-not-found)
            (asserts! (get is-active collateral-reserve) err-not-found)
            
            ;; Update indexes
            (try! (update-indexes debt-token))
            (try! (update-indexes collateral-token))
            
            (let (
                (updated-debt-reserve (unwrap! (map-get? reserves { token: debt-token }) err-not-found))
                (updated-collateral-reserve (unwrap! (map-get? reserves { token: collateral-token }) err-not-found))
                (collateral-value (get-collateral-value user))
                (debt-value (get-debt-value user))
                (health-factor (calculate-health-factor collateral-value debt-value))
                (liquidation-debt (if (< debt-amount (/ (* (get amount user-debt) LIQUIDATION_CLOSE_FACTOR) u10000))
                                    debt-amount
                                    (/ (* (get amount user-debt) LIQUIDATION_CLOSE_FACTOR) u10000)))  ;; min replacement
                )
                (begin
                    ;; Check if position is liquidatable (health factor < 1)
                    (asserts! (< health-factor RAY) err-liquidation)
                    
                    ;; Calculate collateral to seize (with bonus)
                    (let (
                        (debt-price (get-token-price debt-token))
                        (collateral-price (get-token-price collateral-token))
                        (debt-value-calc (* liquidation-debt debt-price))
                        (collateral-to-seize (/ (* debt-value-calc LIQUIDATION_BONUS) (* collateral-price u10000)))
                        )
                        (begin
                            (asserts! (<= collateral-to-seize (get amount user-collateral-amount)) err-insufficient-balance)
                            
                            ;; Repay debt
                            (try! (repay debt-token liquidation-debt))
                            
                            ;; Transfer collateral to liquidator (simplified - in real impl would use transfers)
                            (print { 
                                event: "liquidation",
                                user: user,
                                liquidator: liquidator,
                                debt-token: debt-token,
                                debt-amount: liquidation-debt,
                                collateral-token: collateral-token,
                                collateral-amount: collateral-to-seize,
                                bonus: LIQUIDATION_BONUS
                            })
                            
                            (ok true)
                        )
                    )
                )
            )
        )
    )
)

;; Price Oracle Functions
(define-public (set-token-price (token principal) (price uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-unauthorized)
        (match (map-get? token-prices { token: token })
            existing (map-set token-prices { token: token }
                (merge existing { price: price, last-update: burn-block-height }))
            (map-set token-prices { token: token }
                { price: price, decimals: u8, last-update: burn-block-height })
        )
        (ok true)
    )
)

(define-read-only (get-token-price (token principal))
    (default-to u0 (get price (map-get? token-prices { token: token })))
)

;; Health Factor Calculation
(define-read-only (calculate-health-factor (collateral-value uint) (debt-value uint))
    (if (> debt-value u0)
        (/ (* collateral-value RAY) debt-value)
        u340282366920938463463374607431768211455  ;; Max uint value (2^128 - 1)
    )
)

(define-read-only (get-collateral-value (user principal))
    (let ((total-value u0))
        ;; Iterate through reserves (simplified - in production would iterate through all)
        total-value
    )
)

(define-read-only (get-debt-value (user principal))
    (let ((total-debt u0))
        ;; Iterate through reserves (simplified - in production would iterate through all)
        total-debt
    )
)

(define-read-only (get-user-account-data (user principal))
    (ok {
        total-collateral: (get-collateral-value user),
        total-debt: (get-debt-value user),
        health-factor: (calculate-health-factor (get-collateral-value user) (get-debt-value user))
    })
)

(define-read-only (get-reserve-data (token principal))
    (map-get? reserves { token: token })
)

(define-read-only (get-reserve-list)
    (let ((count (var-get reserves-count))
          (result (list)))
        ;; Build list of reserve addresses
        result
    )
)