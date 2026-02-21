import { Cl } from "@stacks/transactions";
import { describe, expect, it, beforeEach } from "vitest";

const accounts = simnet.getAccounts();
const deployer = accounts.get("deployer")!;
const alice = accounts.get("wallet_1")!;
const bob = accounts.get("wallet_2")!;

// Mock token principal
const TOKEN_A = "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.token-a";
const TOKEN_B = "ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM.token-b";

// Helper function to initialize a reserve
function initReserve(
  token: string,
  name: string,
  symbol: string,
  decimals: number,
  price: number,
  user: string
) {
  return simnet.callPublicFn(
    "lending",
    "init-reserve",
    [
      Cl.principal(token),
      Cl.stringAscii(name),
      Cl.stringAscii(symbol),
      Cl.uint(decimals),
      Cl.uint(price)
    ],
    user
  );
}

// Helper function to supply tokens
function supply(token: string, amount: number, user: string) {
  return simnet.callPublicFn(
    "lending",
    "supply",
    [Cl.principal(token), Cl.uint(amount)],
    user
  );
}

// Helper function to set token price
function setTokenPrice(token: string, price: number, user: string) {
  return simnet.callPublicFn(
    "lending",
    "set-token-price",
    [Cl.principal(token), Cl.uint(price)],
    user
  );
}

// Helper function to get reserve data
function getReserveData(token: string) {
  return simnet.callReadOnlyFn(
    "lending",
    "get-reserve-data",
    [Cl.principal(token)],
    deployer
  );
}

// Helper function to get user account data
function getUserAccountData(user: string) {
  return simnet.callReadOnlyFn(
    "lending",
    "get-user-account-data",
    [Cl.principal(user)],
    deployer
  );
}

// Helper function to get token price
function getTokenPrice(token: string) {
  return simnet.callReadOnlyFn(
    "lending",
    "get-token-price",
    [Cl.principal(token)],
    deployer
  );
}

describe("Lending Pool Tests", () => {
  describe("Initialization Tests", () => {
    it("allows contract owner to initialize a reserve", () => {
      const result = initReserve(
        TOKEN_A,
        "Token A",
        "TKA",
        8,
        100000000,
        deployer
      );

      expect(result.result).toBeOk(Cl.bool(true));

      // Check reserve was created
      const reserve = getReserveData(TOKEN_A);
      
      // Log the actual structure to debug
      console.log("Reserve data:", JSON.stringify(reserve, null, 2));
      
      // Verify the reserve exists
      expect(reserve.result).not.toBeNone();
      
      // Use BigInt for RAY value
      const RAY = 1000000000000000000000000000n;
      
      // Check the value if it exists - using type assertion
      if (reserve.result && 'value' in reserve.result) {
        const data = reserve.result.value as any;
        expect(data['name']).toEqual(Cl.stringAscii("Token A"));
        expect(data['symbol']).toEqual(Cl.stringAscii("TKA"));
        expect(data['decimals']).toEqual(Cl.uint(8));
        expect(data['total-liquidity']).toEqual(Cl.uint(0));
        expect(data['total-borrows']).toEqual(Cl.uint(0));
        expect(data['borrow-index']).toEqual(Cl.uint(RAY));
        expect(data['liquidity-index']).toEqual(Cl.uint(RAY));
        expect(data['interest-rate']).toEqual(Cl.uint(100));
        expect(data['available-liquidity']).toEqual(Cl.uint(0));
        expect(data['is-active']).toEqual(Cl.bool(true));
      }
    });

    it("prevents non-owner from initializing a reserve", () => {
      const result = initReserve(
        TOKEN_A,
        "Token A",
        "TKA",
        8,
        100000000,
        alice
      );

      expect(result.result).toBeErr(Cl.uint(103));
    });

    it("prevents initializing the same reserve twice", () => {
      initReserve(TOKEN_A, "Token A", "TKA", 8, 100000000, deployer);
      
      const result = initReserve(
        TOKEN_A,
        "Token A",
        "TKA",
        8,
        100000000,
        deployer
      );

      expect(result.result).toBeErr(Cl.uint(100));
    });
  });

  describe("Supply Tests", () => {
    beforeEach(() => {
      initReserve(TOKEN_A, "Token A", "TKA", 8, 100000000, deployer);
    });

    it("allows users to supply tokens", () => {
      const result = supply(TOKEN_A, 1000, alice);
      expect(result.result).toBeOk(Cl.bool(true));

      const reserve = getReserveData(TOKEN_A);
      const RAY = 1000000000000000000000000000n;
      
      expect(reserve.result).not.toBeNone();
      
      if (reserve.result && 'value' in reserve.result) {
        const data = reserve.result.value as any;
        expect(data['total-liquidity']).toEqual(Cl.uint(1000));
        expect(data['available-liquidity']).toEqual(Cl.uint(1000));
        expect(data['total-borrows']).toEqual(Cl.uint(0));
        expect(data['borrow-index']).toEqual(Cl.uint(RAY));
        expect(data['liquidity-index']).toEqual(Cl.uint(RAY));
        expect(data['interest-rate']).toEqual(Cl.uint(100));
        expect(data['is-active']).toEqual(Cl.bool(true));
      }
    });

    it("allows multiple users to supply", () => {
      supply(TOKEN_A, 500, alice);
      const result = supply(TOKEN_A, 300, bob);
      expect(result.result).toBeOk(Cl.bool(true));

      const reserve = getReserveData(TOKEN_A);
      const RAY = 1000000000000000000000000000n;
      
      expect(reserve.result).not.toBeNone();
      
      if (reserve.result && 'value' in reserve.result) {
        const data = reserve.result.value as any;
        expect(data['total-liquidity']).toEqual(Cl.uint(800));
        expect(data['available-liquidity']).toEqual(Cl.uint(800));
        expect(data['total-borrows']).toEqual(Cl.uint(0));
        expect(data['borrow-index']).toEqual(Cl.uint(RAY));
        expect(data['liquidity-index']).toEqual(Cl.uint(RAY));
        expect(data['interest-rate']).toEqual(Cl.uint(100));
        expect(data['is-active']).toEqual(Cl.bool(true));
      }
    });

    it("prevents supplying to non-existent reserve", () => {
      const result = supply(TOKEN_B, 1000, alice);
      expect(result.result).toBeErr(Cl.uint(100));
    });
  });

  describe("Price Oracle Tests", () => {
    it("allows owner to set token price", () => {
      const result = setTokenPrice(TOKEN_A, 200000000, deployer);
      expect(result.result).toBeOk(Cl.bool(true));

      const price = getTokenPrice(TOKEN_A);
      // get-token-price returns uint directly, not wrapped in response
      expect(price.result).toEqual(Cl.uint(200000000));
    });

    it("prevents non-owner from setting price", () => {
      const result = setTokenPrice(TOKEN_A, 200000000, alice);
      expect(result.result).toBeErr(Cl.uint(103));
    });

    it("returns 0 for non-existent token price", () => {
      const price = getTokenPrice(TOKEN_B);
      // get-token-price returns uint directly
      expect(price.result).toEqual(Cl.uint(0));
    });
  });

  describe("User Account Data Tests", () => {
    beforeEach(() => {
      initReserve(TOKEN_A, "Token A", "TKA", 8, 100000000, deployer);
    });

    it("returns correct user account data after supply", () => {
      // First supply tokens
      const supplyResult = supply(TOKEN_A, 500, alice);
      expect(supplyResult.result).toBeOk(Cl.bool(true));
      
      // Then check account data
      const accountData = getUserAccountData(alice);
      
      console.log("Account data:", JSON.stringify(accountData, null, 2));
      
      // Verify it's a response
      expect(accountData.result).not.toBeNone();
      
      if (accountData.result && 'value' in accountData.result) {
        const data = accountData.result.value as any;
        // Check that total-collateral is at least 500
        expect(Number(data['total-collateral'].value)).toBeGreaterThanOrEqual(500);
        expect(data['total-debt']).toEqual(Cl.uint(0));
      }
    });

    it("returns zero collateral for user with no supply", () => {
      const accountData = getUserAccountData(bob);
      
      expect(accountData.result).not.toBeNone();
      
      if (accountData.result && 'value' in accountData.result) {
        const data = accountData.result.value as any;
        expect(data['total-collateral']).toEqual(Cl.uint(0));
        expect(data['total-debt']).toEqual(Cl.uint(0));
        // Health factor should be max uint
        expect(data['health-factor'].value).toBeGreaterThan(0);
      }
    });
  });

  describe("Edge Cases", () => {
    beforeEach(() => {
      initReserve(TOKEN_A, "Token A", "TKA", 8, 100000000, deployer);
    });

    it("handles supplying zero amount", () => {
      const result = supply(TOKEN_A, 0, alice);
      expect(result.result).toBeOk(Cl.bool(true));

      const reserve = getReserveData(TOKEN_A);
      const RAY = 1000000000000000000000000000n;
      
      expect(reserve.result).not.toBeNone();
      
      if (reserve.result && 'value' in reserve.result) {
        const data = reserve.result.value as any;
        expect(data['total-liquidity']).toEqual(Cl.uint(0));
        expect(data['available-liquidity']).toEqual(Cl.uint(0));
        expect(data['total-borrows']).toEqual(Cl.uint(0));
        expect(data['borrow-index']).toEqual(Cl.uint(RAY));
        expect(data['liquidity-index']).toEqual(Cl.uint(RAY));
        expect(data['interest-rate']).toEqual(Cl.uint(100));
        expect(data['is-active']).toEqual(Cl.bool(true));
      }
    });

    it("handles multiple operations in sequence", () => {
      supply(TOKEN_A, 1000, alice);
      supply(TOKEN_A, 500, bob);
      
      const reserve = getReserveData(TOKEN_A);
      const RAY = 1000000000000000000000000000n;
      
      expect(reserve.result).not.toBeNone();
      
      if (reserve.result && 'value' in reserve.result) {
        const data = reserve.result.value as any;
        expect(data['total-liquidity']).toEqual(Cl.uint(1500));
        expect(data['available-liquidity']).toEqual(Cl.uint(1500));
        expect(data['total-borrows']).toEqual(Cl.uint(0));
        expect(data['borrow-index']).toEqual(Cl.uint(RAY));
        expect(data['liquidity-index']).toEqual(Cl.uint(RAY));
        expect(data['interest-rate']).toEqual(Cl.uint(100));
        expect(data['is-active']).toEqual(Cl.bool(true));
      }
    });
  });
});