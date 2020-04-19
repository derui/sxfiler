import { ClientResolverLike, RPCClientLike, AppClientLike } from "./client-resolver";

/**
   This module provides mock for the ClientResolverLike. DO NOT USE OUT OF TEST SOURCE.
 */

const dispatcherMock = function createDispatcherMock() {
  return {
    dispatch: jest.fn(),
  };
};

const rpcClientMock = function createApiClientMock() {
  return {
    use: jest.fn(),
  };
};

const appClientMock = function createAppClientMock() {
  return {
    quit: jest.fn(),
  };
};

const clientResolverMock = function createClientResolverMock(rpcClient: RPCClientLike, appClient: AppClientLike) {
  return {
    rpcClient: () => rpcClient,
    appClient: () => appClient,
  } as ClientResolverLike;
};

export const createResolverMocks = function createMocks() {
  const rpcClient = rpcClientMock();
  const appClient = appClientMock();

  return {
    dispatcher: dispatcherMock(),
    rpcClient,
    appClient,
    clientResolver: clientResolverMock(rpcClient, appClient),
  };
};
