

type token_id = nat

type ledger = (token_id, address) big_map

type admin_storage = {
  admin : address;
  pending_admin : address option;
}
type admin_entrypoints =
  | Set_admin of address
  | Confirm_admin of unit

let confirm_new_admin (storage : admin_storage) : admin_storage =
  match storage.pending_admin with
  | None -> (failwith "NO_PENDING_ADMIN" : admin_storage)
  | Some pending ->
    if Tezos.sender = pending
    then { storage with
      pending_admin = (None : address option);
      admin = Tezos.sender;
    }
    else (failwith "NOT_A_PENDING_ADMIN" : admin_storage)
  
(* Fails if sender is not admin *)
let fail_if_not_admin_ext (storage, extra_msg : admin_storage * string) : unit =
  if Tezos.sender <> storage.admin
  then failwith ("NOT_AN_ADMIN" ^  " "  ^ extra_msg)
  else unit

(* Fails if sender is not admin *)
let fail_if_not_admin (storage : admin_storage) : unit =
  if Tezos.sender <> storage.admin
  then failwith "NOT_AN_ADMIN"
  else unit

(* Returns true if sender is admin *)
let is_admin (storage : admin_storage) : bool = Tezos.sender = storage.admin

(*Only callable by admin*)
let set_admin (new_admin, storage : address * admin_storage) : admin_storage =
  let u = fail_if_not_admin storage in
  { storage with pending_admin = Some new_admin; }

let admin_main(param, storage : admin_entrypoints * admin_storage)
    : (operation list) * admin_storage =
  match param with
  | Set_admin new_admin ->
      let new_s = set_admin (new_admin, storage) in
      (([] : operation list), new_s)

  | Confirm_admin u ->
      let new_s = confirm_new_admin storage in
      (([]: operation list), new_s)


type transfer_destination =
[@layout:comb]
{
  to_ : address;
  token_id : token_id;
  amount : nat;
}

type transfer =
[@layout:comb]
{
  from_ : address;
  txs : transfer_destination list;
}

type balance_of_request =
[@layout:comb]
{
  owner : address;
  token_id : token_id;
}

type balance_of_response =
[@layout:comb]
{
  request : balance_of_request;
  balance : nat;
}

type balance_of_param =
[@layout:comb]
{
  requests : balance_of_request list;
  callback : (balance_of_response list) contract;
}

type operator_param =
[@layout:comb]
{
  owner : address;
  operator : address;
  token_id: token_id;
}

type update_operator =
[@layout:comb]
  | Add_operator of operator_param
  | Remove_operator of operator_param

(*
type token_metadata =
[@layout:comb]
{
  token_id : token_id;
  symbol : string;
  name : string;
  decimals : nat;
  extras : (string, string) map;
}
*)

(* todo: it sounds like token_metadata is an extra level of indirection, and should
be replaced by token_info:
https://tzip.tezosagora.org/proposal/tzip-12/#token-metadata
See the section "Token Metadata Storage & Access" -> "Basic"
*)
type token_metadata =
[@layout:comb]
  {
    token_id: token_id;
    token_info: ((string, bytes) map);
  }

type token_metadata_param =
[@layout:comb]
{
  token_ids : token_id list;
  handler : (token_metadata list) -> unit;
}

(*
One of the options to make token metadata discoverable is to declare
`token_metadata : token_metadata_storage` field inside the FA2 contract storage
*)
type token_metadata_storage = (token_id, token_metadata) big_map

(**
token_id ↦ unit
Has the admin been revoked from behaving as an operator?
*)
type admin_operation_storage = (token_id, unit) big_map

(**
A command to modify the admin_operation_storage
*)
type admin_operation_param = 
[@layout:comb]
{
  token_id: token_id;
}

type admin_operation_update =
[@layout:comb]
  | Revoke_admin_operation of admin_operation_param
  | Reinstate_admin_operation of admin_operation_param

type population_lowering =
[@layout:comb]
{
  population: nat;
}

type fa2_entry_points =
  | Balance_of of balance_of_param
  | Update_operators of update_operator list
  (* | Token_metadata_registry of address contract *)

type fa2_token_metadata =
  | Token_metadata of token_metadata_param

(* permission policy definition *)

(* max 35 chars for Tezos annotations *)
type operator_transfer_policy =
  [@layout:comb]
  | No_transfer
  | Owner_transfer
  | Owner_or_operator_transfer
  | Owner_or_operator_or_admin_tx

type owner_hook_policy =
  [@layout:comb]
  | Owner_no_hook
  | Optional_owner_hook
  | Required_owner_hook

type custom_permission_policy =
[@layout:comb]
{
  tag : string;
  config_api: address option;
}

type permissions_descriptor =
[@layout:comb]
{
  operator : operator_transfer_policy;
  receiver : owner_hook_policy;
  sender : owner_hook_policy;
  custom : custom_permission_policy option;
}

(* permissions descriptor entrypoint
type fa2_entry_points_custom =
  ...
  | Permissions_descriptor of permissions_descriptor contract

*)


type transfer_destination_descriptor =
[@layout:comb]
{
  to_ : address option;
  token_id : token_id;
  amount : nat;
}

type transfer_descriptor =
[@layout:comb]
{
  from_ : address option;
  txs : transfer_destination_descriptor list
}

type transfer_descriptor_param =
[@layout:comb]
{
  batch : transfer_descriptor list;
  operator : address;
}

(*
Entrypoints for sender/receiver hooks

type fa2_token_receiver =
  ...
  | Tokens_received of transfer_descriptor_param

type fa2_token_sender =
  ...
  | Tokens_sent of transfer_descriptor_param
*)


(** One of the specified `token_id`s is not defined within the FA2 contract *)
let fa2_token_undefined = "FA2_TOKEN_UNDEFINED" 
(** 
A token owner does not have sufficient balance to transfer tokens from
owner's account 
*)
let fa2_insufficient_balance = "FA2_INSUFFICIENT_BALANCE"
(** A transfer failed because of `operator_transfer_policy == No_transfer` *)
let fa2_tx_denied = "FA2_TX_DENIED"
(** 
A transfer failed because `operator_transfer_policy == Owner_transfer` and it is
initiated not by the token owner 
*)
let fa2_not_owner = "FA2_NOT_OWNER"
(**
A transfer failed because {`operator_transfer_policy == Owner_or_operator_transfer`
or `operator_transfer_policy == Owner_or_operator_or_admin_tx`}; and because
it was initiated neither by the token owner, nor a permitted operator, nor an admin
 *)
let fa2_not_operator = "FA2_NOT_OPERATOR"
(** 
`update_operators` entrypoint is invoked and `operator_transfer_policy` is
`No_transfer` or `Owner_transfer`
*)
let fa2_operators_not_supported = "FA2_OPERATORS_UNSUPPORTED"
(**
Receiver hook is invoked and failed. This error MUST be raised by the hook
implementation
 *)
let fa2_receiver_hook_failed = "FA2_RECEIVER_HOOK_FAILED"
(**
Sender hook is invoked and failed. This error MUST be raised by the hook
implementation
 *)
let fa2_sender_hook_failed = "FA2_SENDER_HOOK_FAILED"
(**
Receiver hook is required by the permission behavior, but is not implemented by
a receiver contract
 *)
let fa2_receiver_hook_undefined = "FA2_RECEIVER_HOOK_UNDEFINED"
(**
Sender hook is required by the permission behavior, but is not implemented by
a sender contract
 *)
let fa2_sender_hook_undefined = "FA2_SENDER_HOOK_UNDEFINED"


let revoke_admin_operator (token_id, admin_op_storage, ledger, admin : token_id * admin_operation_storage * ledger * address)
    : admin_operation_storage =
  match Big_map.find_opt token_id ledger with
      | None -> (failwith fa2_not_owner : admin_operation_storage)
      | Some owner ->
        if owner = Tezos.sender || admin = Tezos.sender
        then Big_map.update token_id (Some unit) admin_op_storage
        else (failwith fa2_not_owner : admin_operation_storage)

let reinstate_admin_operator (token_id, admin_op_storage, ledger, admin : token_id * admin_operation_storage * ledger * address)
    : admin_operation_storage =
  match Big_map.find_opt token_id ledger with
    | None -> (failwith fa2_not_owner : admin_operation_storage)
    | Some owner ->
      if owner = Tezos.sender
      then Big_map.remove token_id admin_op_storage
      else (failwith fa2_not_owner : admin_operation_storage)

let update_admin_operator (update, admin_op_storage, ledger, admin : admin_operation_update * admin_operation_storage * ledger * address)
    : admin_operation_storage =
  match update with
  | Revoke_admin_operation op ->
    revoke_admin_operator(op.token_id, admin_op_storage, ledger, admin)
  | Reinstate_admin_operation op ->
    reinstate_admin_operator(op.token_id, admin_op_storage, ledger, admin)

(** 
Reference implementation of the FA2 operator storage, config API and 
helper functions 
*)

(** 
(owner, operator, token_id) -> unit
To be part of FA2 storage to manage permitted operators
*)
type operator_storage = ((address * (address * token_id)), unit) big_map

(** 
  Updates operator storage using an `update_operator` command.
  Helper function to implement `Update_operators` FA2 entrypoint
*)
let update_operators (update, storage : update_operator * operator_storage)
    : operator_storage =
  match update with
  | Add_operator op -> 
    Big_map.update (op.owner, (op.operator, op.token_id)) (Some unit) storage
  | Remove_operator op -> 
    Big_map.remove (op.owner, (op.operator, op.token_id)) storage

(**
Validate if operator update is performed by the token owner.
@param caller an address that initiated the operation; usually `Tezos.sender`.
*)
let validate_update_operators_by_owner (update, caller : update_operator * address)
    : unit =
  let op = match update with
  | Add_operator op -> op
  | Remove_operator op -> op
  in
  if op.owner = caller then unit else failwith fa2_not_owner

(**
  Generic implementation of the FA2 `%update_operators` entrypoint.
  Assumes that only the token owner can change its operators.
 *)
let fa2_update_operators (updates, storage
    : (update_operator list) * operator_storage) : operator_storage =
  let updater = Tezos.sender in
  let process_update = (fun (ops, update : operator_storage * update_operator) ->
    let u = validate_update_operators_by_owner (update, updater) in
    update_operators (update, ops)
  ) in
  List.fold process_update updates storage

(** 
  owner * operator * token_id * ops_storage * admin * admin_ops_storage -> unit
*)
type operator_validator = (address * address * token_id * operator_storage * address * admin_operation_storage)-> unit

(**
Create an operator validator function based on provided operator policy.
@param tx_policy operator_transfer_policy defining the constrains on who can transfer.
@return (owner, operator, token_id, ops_storage) -> unit
todo: delete
 *)
let make_operator_validator (tx_policy : operator_transfer_policy) : operator_validator =
  let can_owner_tx, can_operator_tx, can_admin_tx = match tx_policy with
  | No_transfer -> (failwith fa2_tx_denied : bool * bool * bool)
  | Owner_transfer -> true, false, false
  | Owner_or_operator_transfer -> true, true, false
  | Owner_or_operator_or_admin_tx -> true, true, true
  in
  (fun (owner, caller, token_id, ops_storage, admin, admin_ops_storage
      : address * address * token_id * operator_storage * address * admin_operation_storage) ->
    if can_owner_tx && owner = caller
    then unit (* transfer by the owner *)
    else if can_operator_tx && Big_map.mem (owner, (caller, token_id)) ops_storage
    then unit
    else if can_admin_tx && admin = caller && Big_map.mem token_id admin_ops_storage
    then failwith fa2_not_operator  (* A key in this map means admin is revoked *)
    else if can_admin_tx && admin = caller
    then unit
    else if can_operator_tx || can_admin_tx
    then failwith fa2_not_operator (* operators are allowed to transfer, but this caller is not an operator *)
    else failwith fa2_not_owner (* operators cannot transfer; and this caller is not the owner *)
  )

(**
Default implementation of the operator validation function.
Notice that this function complies to an `operator_transfer_policy` of `Owner_or_operator_or_admin_tx`
 *)
let default_operator_validator : operator_validator =
  (fun (owner, caller, token_id, ops_storage, admin, admin_ops_storage 
      : address * address * token_id * operator_storage * address * admin_operation_storage) ->
    if owner = caller
    then unit (* transfer by the owner *)
    else if Big_map.mem (owner, (caller, token_id)) ops_storage
    then unit (* the operator is permitted for the token_id *)
    else if caller = admin && Big_map.mem token_id admin_ops_storage
    then failwith fa2_not_operator  (* the entry in the big_map indicates that the admin has been revoked as an operator *)
    else if caller = admin
    then unit
    else failwith fa2_not_operator (* the operator is not permitted for the token_id *)
  )

(** 
Validate operators for all transfers in the batch at once
@param tx_policy operator_transfer_policy defining the constrains on who can transfer.
*)
let validate_operator (tx_policy, txs, ops_storage, admin, admin_ops_storage
    : operator_transfer_policy * (transfer list) * operator_storage * address * admin_operation_storage) : unit =
  let validator = make_operator_validator tx_policy in
  List.iter (fun (tx : transfer) -> 
    List.iter (fun (dst: transfer_destination) ->
      validator (tx.from_, Tezos.sender, dst.token_id, ops_storage, admin, admin_ops_storage)
    ) tx.txs
  ) txs

(**
Generic implementation of the permission logic for sender and receiver hooks.
Actual behavior is driven by a `permissions_descriptor`.
To be used in FA2 and/or FA2 permission transfer hook contract implementation
which supports sender/receiver hooks.
*)


type get_owners = transfer_descriptor -> (address option) list

type hook_entry_point = transfer_descriptor_param contract

type hook_result =
  | Hook_entry_point of hook_entry_point
  | Hook_undefined of string

type to_hook = address -> hook_result

(**
Extracts a set of unique `from_` or `to_` addresses from the transfer batch.
@param batch transfer batch
@param get_owner selector of `from_` or `to_` addresses from each individual `transfer_descriptor`
 *)
let get_owners_from_batch (batch, get_owners : (transfer_descriptor list) * get_owners) : address set =
  List.fold
    (fun (acc, tx : (address set) * transfer_descriptor) ->
      let owners = get_owners tx in
      List.fold
        (fun (acc, o: (address set) * (address option)) ->
          match o with
          | None -> acc
          | Some a -> Set.add a acc
        )
        owners
        acc
    )
    batch
    (Set.empty : address set)

let validate_owner_hook (p, get_owners, to_hook, is_required :
    transfer_descriptor_param * get_owners * to_hook * bool)
    : hook_entry_point list =
  let owners = get_owners_from_batch (p.batch, get_owners) in
  Set.fold
    (fun (eps, owner : (hook_entry_point list) * address) ->
      match to_hook owner with
      | Hook_entry_point h -> h :: eps
      | Hook_undefined error ->
        (* owner hook is not implemented by the target contract *)
        if is_required
        then (failwith error : hook_entry_point list) (* owner hook is required: fail *)
        else eps (* owner hook is optional: skip it *)
      )
    owners ([] : hook_entry_point list)

let validate_owner(p, policy, get_owners, to_hook :
    transfer_descriptor_param * owner_hook_policy * get_owners * to_hook)
    : hook_entry_point list =
  match policy with
  | Owner_no_hook -> ([] : hook_entry_point list)
  | Optional_owner_hook -> validate_owner_hook (p, get_owners, to_hook, false)
  | Required_owner_hook -> validate_owner_hook (p, get_owners, to_hook, true)

(**
Given an address of the token receiver, tries to get an entrypoint for
`fa2_token_receiver` interface.
 *)
let to_receiver_hook : to_hook = fun (a : address) ->
    let c : hook_entry_point option =
    Tezos.get_entrypoint_opt "%tokens_received" a in
    match c with
    | Some c -> Hook_entry_point c
    | None -> Hook_undefined fa2_receiver_hook_undefined

(**
Create a list iof Tezos operations invoking all token receiver contracts that
implement `fa2_token_receiver` interface. Fail if specified `owner_hook_policy`
cannot be met.
 *)
let validate_receivers (p, receiver_policy : transfer_descriptor_param * owner_hook_policy)
    : hook_entry_point list =
  let get_receivers : get_owners = fun (tx : transfer_descriptor) ->
    List.map (fun (t : transfer_destination_descriptor) -> t.to_ )tx.txs in
  validate_owner (p, receiver_policy, get_receivers, to_receiver_hook)

(**
Given an address of the token sender, tries to get an entrypoint for
`fa2_token_sender` interface.
 *)
let to_sender_hook : to_hook = fun (a : address) ->
    let c : hook_entry_point option =
    Tezos.get_entrypoint_opt "%tokens_sent" a in
    match c with
    | Some c -> Hook_entry_point c
    | None -> Hook_undefined fa2_sender_hook_undefined

(**
Create a list iof Tezos operations invoking all token sender contracts that
implement `fa2_token_sender` interface. Fail if specified `owner_hook_policy`
cannot be met.
 *)
let validate_senders (p, sender_policy : transfer_descriptor_param * owner_hook_policy)
    : hook_entry_point list =
  let get_sender : get_owners = fun (tx : transfer_descriptor) -> [tx.from_] in
  validate_owner (p, sender_policy, get_sender, to_sender_hook)

(**
Generate a list of Tezos operations invoking sender and receiver hooks according to
the policies defined by the permissions descriptor.
To be used in FA2 and/or FA2 transfer hook contract implementation which supports
sender/receiver hooks.
 *)
let get_owner_transfer_hooks (p, descriptor : transfer_descriptor_param * permissions_descriptor)
    : hook_entry_point list =
  let sender_entries = validate_senders (p, descriptor.sender) in
  let receiver_entries = validate_receivers (p, descriptor.receiver) in
  (* merge two lists *)
  List.fold
    (fun (l, ep : (hook_entry_point list) * hook_entry_point) -> ep :: l)
    receiver_entries sender_entries

let transfers_to_descriptors (txs : transfer list) : transfer_descriptor list =
  List.map
    (fun (tx : transfer) ->
      let txs = List.map
        (fun (dst : transfer_destination) ->
          {
            to_ = Some dst.to_;
            token_id = dst.token_id;
            amount = dst.amount;
          }
        ) tx.txs in
        {
          from_ = Some tx.from_;
          txs = txs;
        }
    ) txs

let transfers_to_transfer_descriptor_param
    (txs, operator : (transfer list) * address) : transfer_descriptor_param =
  {
    batch = transfers_to_descriptors txs;
    operator = operator;
  }

(**
 Gets operations to call sender/receiver hook for the specified transfer and
 permission descriptor
 *)
let get_owner_hook_ops_for (tx_descriptor, pd
    : transfer_descriptor_param * permissions_descriptor) : operation list =
  let hook_calls = get_owner_transfer_hooks (tx_descriptor, pd) in
  match hook_calls with
  | [] -> ([] : operation list)
  | h :: t ->
    List.map (fun(call: hook_entry_point) ->
      Tezos.transaction tx_descriptor 0mutez call)
      hook_calls


type nft_meta = (token_id, token_metadata) big_map


type nft_token_storage = {
  ledger : ledger;
  token_metadata : nft_meta;
  next_token_id : token_id;
  max_population: nat;
  operators : operator_storage;
  admin_operators: admin_operation_storage;
}


let dec_balance(owner, token_id, ledger : address option * token_id * ledger) : ledger =
  match owner with
  | None -> ledger (* this is mint transfer, don't change the ledger *)
  | Some o -> (
    let current_owner = Big_map.find_opt token_id ledger in
    match current_owner with
    | None -> (failwith fa2_token_undefined : ledger)
    | Some cur_o ->
      if cur_o = o
      then Big_map.remove token_id ledger
      else (failwith fa2_insufficient_balance : ledger)
  )

let inc_balance(owner, token_id, ledger : address option * token_id * ledger) : ledger =
  match owner with
  | None -> ledger (* this is burn transfer, don't change the ledger *)
  | Some o -> Big_map.add token_id o ledger


(**
Retrieve the balances for the specified tokens and owners
@return callback operation
*)
let get_balance (p, ledger : balance_of_param * ledger) : operation =
  let to_balance = fun (r : balance_of_request) ->
    let owner = Big_map.find_opt r.token_id ledger in
    match owner with
    | None -> (failwith fa2_token_undefined : balance_of_response)
    | Some o ->
      let bal = if o = r.owner then 1n else 0n in
      { request = r; balance = bal; }
  in
  let responses = List.map to_balance p.requests in
  Tezos.transaction responses 0mutez p.callback


let fa2_main (param, storage : fa2_entry_points * nft_token_storage)
    : (operation  list) * nft_token_storage =
  match param with
  | Balance_of p ->
    let op = get_balance (p, storage.ledger) in
    [op], storage

  | Update_operators updates ->
    let new_operators = fa2_update_operators (updates, storage.operators) in
    let new_storage = { storage with operators = new_operators; } in
    ([] : operation list), new_storage

  (* | Token_metadata_registry callback ->
   *   (\* the contract storage holds `token_metadata` big_map*\)
   *   let callback_op = Tezos.transaction Tezos.self_address 0mutez callback in
   *   [callback_op], storage *)


type nft_asset_storage = {
  assets : nft_token_storage;
  admin : admin_storage;
  metadata: (string, bytes) big_map; (* contract metadata *)
}

let get_owner_hook_ops (tx_descriptors, storage
    : (transfer_descriptor list) * nft_asset_storage) : operation list =
  ([] : operation list)



(**
Update leger balances according to the specified transfers. Fails if any of the
permissions or constraints are violated.
@param txs transfers to be applied to the ledger
@param validate_op function that validates of the tokens from the particular owner can be transferred.
 *)
let transfer (txs, validate_op, ops_storage, ledger, admin, admin_ops_storage
    : (transfer_descriptor list) * operator_validator * operator_storage * ledger * address * admin_operation_storage)
    : ledger =
  let make_transfer = fun (l, tx : ledger * transfer_descriptor) ->
    List.fold
      (fun (ll, dst : ledger * transfer_destination_descriptor) ->
        let u = match tx.from_ with
        | None -> unit
        | Some owner -> validate_op (owner, Tezos.sender, dst.token_id, ops_storage, admin, admin_ops_storage)
        in
        if dst.amount > 1n
        then (failwith fa2_insufficient_balance : ledger)
        else if dst.amount = 0n
        then match Big_map.find_opt dst.token_id ll with
               | None -> (failwith fa2_token_undefined : ledger)
               | Some cur_o -> ll (* zero transfer, don't change the ledger *)
        else
          let lll = dec_balance (tx.from_, dst.token_id, ll) in
          inc_balance(dst.to_, dst.token_id, lll)
      ) tx.txs l
  in
  List.fold make_transfer txs ledger


let fa2_transfer (tx_descriptors, validate_op, storage
    : (transfer_descriptor list) * operator_validator * nft_asset_storage)
    : (operation list) * nft_asset_storage =

  let assets = storage.assets in
  let new_ledger = transfer (tx_descriptors, validate_op, assets.operators, assets.ledger, storage.admin.admin, assets.admin_operators) in
  let new_storage = { storage with assets.ledger = new_ledger; } in
  let ops = get_owner_hook_ops (tx_descriptors, storage) in
  ops, new_storage


type minted1 = {
  storage : nft_token_storage;
  reversed_txs : transfer_destination_descriptor list;
}


type mint_token_param =
[@layout:comb]
{
  token_metadata: token_metadata;
  owner : address;
}

type mint_tokens_param = mint_token_param list

let update_meta_and_create_txs (param, storage
    : mint_tokens_param * nft_token_storage ) : minted1 =
  let seed1 : minted1 = {
    storage = storage;
    reversed_txs = ([] : transfer_destination_descriptor list);
  } in
  List.fold
    (fun (acc, t : minted1 * mint_token_param) ->
      let new_token_id = t.token_metadata.token_id in
      if (Big_map.mem new_token_id acc.storage.ledger)
      then (failwith "FA2_INVALID_TOKEN_ID" : minted1)
      else if new_token_id > acc.storage.max_population && acc.storage.max_population > 0n
      then (failwith "FA2_MAX_POPULATION_REACHED" : minted1)
      else
        let new_token_metadata = 
          Big_map.add new_token_id t.token_metadata acc.storage.token_metadata in
        let next_token_id : nat = new_token_id + 1n in 
        let new_storage = { acc.storage with
          token_metadata = new_token_metadata;
          next_token_id = next_token_id;
        } in

        let tx : transfer_destination_descriptor = {
          to_ = Some t.owner;
          token_id = new_token_id;
          amount = 1n;
        } in

        {
          storage = new_storage;
          reversed_txs = tx :: acc.reversed_txs;
        }
    ) param seed1

let mint_tokens (param, storage : mint_tokens_param * nft_asset_storage)
    : operation list * nft_asset_storage =
  let mint1 = update_meta_and_create_txs (param, storage.assets) in
  (* update ledger *)
  let tx_descriptor : transfer_descriptor = {
    from_ = (None : address option);
    txs = mint1.reversed_txs;
  } in
  let storage = {storage with assets = mint1.storage;} in
  let nop_operator_validator =
    fun (p : address * address * token_id * operator_storage * address * admin_operation_storage) -> unit in
  let ops, storage = fa2_transfer ([tx_descriptor], nop_operator_validator, storage) in
  ops, storage

type nft_asset_entrypoints =
  | Transfer of transfer list
  | Assets of fa2_entry_points
  | Mint of mint_tokens_param
  | Admin of admin_entrypoints
  | Update_admin_operator of admin_operation_update
  | Decrease_population of population_lowering


let nft_asset_main (param, storage : nft_asset_entrypoints * nft_asset_storage)
    : operation list * nft_asset_storage =
  match param with
  | Transfer txs ->
    let tx_descriptors = transfers_to_descriptors txs in
    (*
    will validate that a sender is either `from_` parameter of each transfer
    or a permitted operator for the owner `from_` address.
    *)
    fa2_transfer (tx_descriptors, default_operator_validator, storage)

  | Assets fa2 ->
    let ops, new_assets = fa2_main (fa2, storage.assets) in
    let new_storage = { storage with assets = new_assets; } in
    ops, new_storage


  | Mint mp ->
    let _ = fail_if_not_admin storage.admin in
    let ops, new_storage = mint_tokens (mp, storage) in
    ops, new_storage


  | Admin a ->
    let ops, admin = admin_main (a, storage.admin) in
    let new_storage = { storage with admin = admin; } in
    ops, new_storage

  | Update_admin_operator update ->
    let assets = storage.assets in
    let new_admin_operators = update_admin_operator (update, assets.admin_operators, assets.ledger, storage.admin.admin) in
    let new_assets = { assets with admin_operators = new_admin_operators; } in
    let new_storage = { storage with assets = new_assets; } in
    ([] : operation list), new_storage

  | Decrease_population lowering ->
    let _ = fail_if_not_admin storage.admin in
    let _ = if lowering.population >= storage.assets.max_population
    then failwith "The population must decrease" in
    let new_assets = { storage.assets with max_population = lowering.population; } in
    let new_storage = { storage with assets = new_assets; } in
    ([] : operation list), new_storage
