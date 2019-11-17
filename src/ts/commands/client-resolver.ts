// This module provides the interface to resolve some of clients in this application.
import { ApiMethod } from "@/apis";
import { Client } from "@/libs/json-rpc/client";

export type ApiClientLike = Client<ApiMethod>;

/**
   The client contains some operations for operating application directly.

   When use this client in a command, affects the running application immediately.
 */
export type AppClientLike = {
  /**
     Quit the current application. *WARNING:* This function quit this application immediately.
   */
  quit(): void;
};

/**
   The interface to resolve some of the clients fro application.
 */
export type ClientResolverLike = {
  /**
     Resolve the application client. Do not save the reference to other place.
   */
  appClient(): AppClientLike;

  /**
     Resolve the application client. Do not save the reference to other place.
   */
  apiClient(): ApiClientLike;
};
