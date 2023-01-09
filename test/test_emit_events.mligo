(* MIT License
   Copyright (c) 2022 Marigold <contact@marigold.dev>
   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal in
   the Software without restriction, including without limitation the rights to
   use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
   the Software, and to permit persons to whom the Software is furnished to do so,
   subject to the following conditions:
   The above copyright notice and this permission notice shall be included in all
   copies or substantial portions of the Software.
   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)

#import "ligo-breathalyzer/lib/lib.mligo" "Breath"
#import "./common/helper.mligo" "Helper"
#import "./common/assert.mligo" "Assert"
#import "./common/mock_contract.mligo" "Mock_contract"
#import "./common/util.mligo" "Util"
#import "../src/internal/proposal_content.mligo" "Proposal_content"
#import "../src/internal/storage.mligo" "Storage"

type proposal_content = Proposal_content.Types.t
type storage_types_proposal = Storage.Types.proposal
type storage_types_proposal_id = Storage.Types.proposal_id

let case_emitted_create_proposal =
  Breath.Model.case
  "test emitted event for creating proposal"
  "successuful emit event"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, _carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address;] in
      let init_storage = Helper.init_storage (owners, 2n) in
      let multisig_contract = Helper.originate level Mock_contract.multisig_main init_storage 0tez in
      let add_contract = Breath.Contract.originate level "add_contr" Mock_contract.add_main 1n 0tez in
      let param = ([] : (nat proposal_content) list) in

      let param1 = (Execute { target = add_contract.originated_address; parameter = 10n; amount = 0tez;} :: param) in
      let action1 = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param1) in
      let multisig_address = multisig_contract.originated_address in

      let events = (Util.get_last_events_from multisig_address "create_proposal" : (storage_types_proposal_id * nat storage_types_proposal) list) in

      let (emitted_id, emitted_proposal) = Option.unopt (List.head_opt events) in

      let storage = Breath.Contract.storage_of multisig_contract in
      let proposal = Util.unopt (Big_map.find_opt 1n storage.proposals) "proposal 1 doesn't exist" in

      Breath.Result.reduce [
        action1
      ; Breath.Assert.is_equal "proposal id" emitted_id 1n
      ; Breath.Assert.is_equal "proposal" emitted_proposal proposal
      ])

let case_emitted_sign_proposal =
  Breath.Model.case
  "test emitted event for signing proposal"
  "successuful emit event"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, _carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address;] in
      let init_storage = Helper.init_storage (owners, 2n) in
      let multisig_contract = Helper.originate level Mock_contract.multisig_main init_storage 0tez in
      let add_contract = Breath.Contract.originate level "add_contr" Mock_contract.add_main 1n 0tez in
      let param = ([] : (nat proposal_content) list) in

      let param1 = (Execute { target = add_contract.originated_address; parameter = 10n; amount = 0tez;} :: param) in
      let action1 = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param1) in
      let pack1 = Helper.pack_proposal_content multisig_contract 1n in
      let sign_action1 = Breath.Context.act_as bob (Helper.sign_proposal_only multisig_contract 1n true pack1) in
      let multisig_address = multisig_contract.originated_address in

      let events = (Util.get_last_events_from multisig_address "sign_proposal" : (storage_types_proposal_id * address * bool) list) in

      let (emitted_proposal_id, emitted_addr, emitted_agreement) = Option.unopt (List.head_opt events) in

      Breath.Result.reduce [
        action1
      ; sign_action1
      ; Breath.Assert.is_equal "proposal id" emitted_proposal_id 1n
      ; Breath.Assert.is_equal "owner" emitted_addr bob.address
      ; Breath.Assert.is_equal "agreement" emitted_agreement true
      ])

let case_emitted_exe_proposal =
  Breath.Model.case
  "test emitted event for executing proposal"
  "successuful emit event"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, _carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address;] in
      let init_storage = Helper.init_storage (owners, 1n) in
      let multisig_contract = Helper.originate level Mock_contract.multisig_main init_storage 0tez in
      let add_contract = Breath.Contract.originate level "add_contr" Mock_contract.add_main 1n 0tez in
      let param = ([] : (nat proposal_content) list) in

      let param1 = (Execute { target = add_contract.originated_address; parameter = 10n; amount = 0tez;} :: param) in
      let action1 = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param1) in
      let pack1 = Helper.pack_proposal_content multisig_contract 1n in
      let sign_action1 = Breath.Context.act_as bob (Helper.sign_proposal_only multisig_contract 1n true pack1) in
      let exe_action1 = Breath.Context.act_as bob (Helper.resolve_proposal multisig_contract 1n pack1) in

      let multisig_address = multisig_contract.originated_address in
      let events = (Util.get_last_events_from multisig_address "resolve_proposal" : (storage_types_proposal_id * address) list) in

      let (emitted_proposal_id, emitted_addr) = Option.unopt (List.head_opt events) in

      Breath.Result.reduce [
        action1
      ; sign_action1
      ; exe_action1
      ; Breath.Assert.is_equal "proposal id" emitted_proposal_id 1n
      ; Breath.Assert.is_equal "owner" emitted_addr bob.address
      ])

let case_emitted_sign_and_resolve_proposal =
  Breath.Model.case
  "test emitted event for signing proposal and executing proposal"
  "successuful emit event"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, _carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address;] in
      let init_storage = Helper.init_storage (owners, 1n) in
      let multisig_contract = Helper.originate level Mock_contract.multisig_main init_storage 0tez in
      let add_contract = Breath.Contract.originate level "add_contr" Mock_contract.add_main 1n 0tez in
      let param = ([] : (nat proposal_content) list) in

      let param1 = (Execute { target = add_contract.originated_address; parameter = 10n; amount = 0tez;} :: param) in
      let action1 = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param1) in
      let pack1 = Helper.pack_proposal_content multisig_contract 1n in
      let sign_action1 = Breath.Context.act_as bob (Helper.sign_and_resolve_proposal multisig_contract 1n true pack1) in
      let multisig_address = multisig_contract.originated_address in

      let sign_events = (Util.get_last_events_from multisig_address "sign_proposal" : (storage_types_proposal_id * address * bool) list) in
      let exe_events = (Util.get_last_events_from multisig_address "resolve_proposal" : (storage_types_proposal_id * address) list) in

      let (emitted_sign_proposal_id, emitted_owner_addr, emitted_agreement) = Option.unopt (List.head_opt sign_events) in
      let (emitted_exe_proposal_id, emitted_exe_addr) = Option.unopt (List.head_opt exe_events) in

      Breath.Result.reduce [
        action1
      ; sign_action1
      ; Breath.Assert.is_equal "sign-proposal id" emitted_sign_proposal_id 1n
      ; Breath.Assert.is_equal "owner address" emitted_owner_addr bob.address
      ; Breath.Assert.is_equal "agreement" emitted_agreement true
      ; Breath.Assert.is_equal "exe-proposal id" emitted_exe_proposal_id 1n
      ; Breath.Assert.is_equal "executor address" emitted_exe_addr bob.address
      ])

let case_emitted_sign_proposal_without_exe_proposal =
  Breath.Model.case
  "test emitted event for signing proposal but without executing proposal"
  "successuful emit event"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, _carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address;] in
      let init_storage = Helper.init_storage (owners, 2n) in
      let multisig_contract = Helper.originate level Mock_contract.multisig_main init_storage 0tez in
      let add_contract = Breath.Contract.originate level "add_contr" Mock_contract.add_main 1n 0tez in
      let param = ([] : (nat proposal_content) list) in

      let param1 = (Execute { target = add_contract.originated_address; parameter = 10n; amount = 0tez;} :: param) in
      let action1 = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param1) in
      let pack1 = Helper.pack_proposal_content multisig_contract 1n in
      let sign_action1 = Breath.Context.act_as bob (Helper.sign_and_resolve_proposal multisig_contract 1n true pack1) in
      let multisig_address = multisig_contract.originated_address in

      let sign_events = (Util.get_last_events_from multisig_address "sign_proposal" : (storage_types_proposal_id * address * bool) list) in

      let (emitted_sign_proposal_id, emitted_owner_addr, emitted_agreement) = Option.unopt (List.head_opt sign_events) in

      Breath.Result.reduce [
        action1
      ; sign_action1
      ; Breath.Assert.is_equal "sign-proposal id" emitted_sign_proposal_id 1n
      ; Breath.Assert.is_equal "owner address" emitted_owner_addr bob.address
      ; Breath.Assert.is_equal "agreement" emitted_agreement true
      ])

let case_emitted_receiving_amount =
  Breath.Model.case
  "test emitted event for receiving amount"
  "successuful emit event"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address;] in
      let init_storage = Helper.init_storage (owners, 2n) in
      let multisig_contract = Helper.originate level Mock_contract.multisig_main init_storage 0tez in
      let contract = Mock_contract.originate_transfer_only_contract level in

      let action = Breath.Context.act_as carol (fun (_u:unit) -> (Breath.Contract.transfert_to contract multisig_contract.originated_address 0tez)) in

      let events = (Util.get_last_events_from multisig_contract.originated_address "receiving_tez" : (address * tez) list) in

      let (emitted_sender, emitted_amount) = Option.unopt (List.head_opt events) in

      Breath.Result.reduce [
        action
      ; Breath.Assert.is_equal "sender" emitted_sender contract.originated_address
      ; Breath.Assert.is_equal "amount" emitted_amount 10tez
      ])

let test_suite =
  Breath.Model.suite "Suite for emitted events" [
    case_emitted_create_proposal
  ; case_emitted_sign_proposal
  ; case_emitted_exe_proposal
  ; case_emitted_sign_and_resolve_proposal
  ; case_emitted_sign_proposal_without_exe_proposal
  ; case_emitted_receiving_amount
  ]

