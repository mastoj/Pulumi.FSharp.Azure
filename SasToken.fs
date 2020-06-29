[<AutoOpen>]
module Pulumi.FSharp.Azure.SasToken

open Pulumi.Azure.Storage

module SasTokenInternal =    
    type StorageBlobArgsRecord = {
        StorageAccount: Account
        Blob: Blob
    }

open SasTokenInternal

type SasTokenBuilder internal () =
    member __.Yield _ = {
        StorageAccount = null
        Blob = null
    }
   
    [<CustomOperation("blob")>]
    member __.Blob(args, blob) =
        { args with Blob = blob }
       
    member __.Run (args) =
         SharedAccessSignature.SignedBlobReadUrl(args.Blob, args.StorageAccount)

    [<CustomOperation("storageAccount")>]
    member __.StorageAccount(args, storageAccount) = {
        args with StorageAccount = storageAccount
    }

let sasToken = SasTokenBuilder()