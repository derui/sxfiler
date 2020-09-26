import { FileItem } from "@/generated/filer_pb";
import { Client } from "@/rpc/client";

// This module provides the interface to resolve some of clients in this application.

export type RPCClientLike = Client;

/**
   The client contains some operations for operating application directly.

   When use this client in a command, affects the running application immediately.
 */
export type AppClientLike = {
  /**
     Quit the current application. *WARNING:* This function quit this application immediately.
   */
  quit(): void;

  openItem(item: FileItem): void;
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
  rpcClient(): RPCClientLike;
};
