import { ClientResolverLike, ApiClientLike, AppClientLike } from "./client-resolver";

/**
   This module provides mock for the ClientResolverLike. DO NOT USE OUT OF TEST SOURCE.
 */

const dispatcherMock = function createDispatcherMock() {
  return {
    dispatch: jest.fn(),
  };
};

const apiClientMock = function createApiClientMock() {
  return {
    call: jest.fn(),
    notify: jest.fn(),
  };
};

const appClientMock = function createAppClientMock() {
  return {
    quit: jest.fn(),
  };
};

const clientResolverMock = function createClientResolverMock(apiClient: ApiClientLike, appClient: AppClientLike) {
  return {
    apiClient: () => apiClient,
    appClient: () => appClient,
  } as ClientResolverLike;
};

export const createResolverMocks = function createMocks() {
  const apiClient = apiClientMock();
  const appClient = appClientMock();

  return {
    dispatcher: dispatcherMock(),
    apiClient,
    appClient,
    clientResolver: clientResolverMock(apiClient, appClient),
  };
};
