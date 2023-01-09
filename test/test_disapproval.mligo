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

type proposal_content = Proposal_content.Types.t

let case_sign_for_disapproval =
  Breath.Model.case
  "test sign proposal with disproposal"
  "successufully sign proposal"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address; carol.address] in
      let init_storage = Helper.init_storage (owners, 1n) in
      let multisig_contract = Helper.originate level Mock_contract.multisig_main init_storage 100tez in
      let add_contract = Breath.Contract.originate level "add_contr" Mock_contract.add_main 1n 0tez in
      let param = ([] : (nat proposal_content) list) in

      let param1 = (Execute { target = add_contract.originated_address; parameter = 10n; amount = 0tez;} :: param) in
      let create_action1 = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param1) in
      let pack1 = Helper.pack_proposal_content multisig_contract 1n in
      let sign_action1 = Breath.Context.act_as bob (Helper.sign_proposal_only multisig_contract 1n false pack1) in

      let balance = Breath.Contract.balance_of multisig_contract in
      let storage = Breath.Contract.storage_of multisig_contract in

      let proposal1 = Util.unopt (Big_map.find_opt 1n storage.proposals) "proposal 1 doesn't exist" in

      Breath.Result.reduce [
        create_action1
      ; sign_action1
      ; Breath.Assert.is_equal "balance" balance 100tez
      ; Breath.Assert.is_equal "the counter of proposal" storage.proposal_counter 1n
      ; Assert.is_proposal_equal "#1 proposal" proposal1
        ({
          state            = Proposing;
          signatures       = Map.literal [(bob.address, false)];
          proposer         = { actor = alice.address; timestamp = Tezos.get_now () };
          resolver         = None;
          contents         = [ Execute {
            amount           = 0tez;
            target           = add_contract.originated_address;
            parameter        = 10n;
          }]
        })
      ])

let case_fail_double_sign =
  Breath.Model.case
  "sign the same proposal twice"
  "fail to sign"
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
      let sign_action2 = Breath.Context.act_as bob (Helper.sign_proposal_only multisig_contract 1n false pack1) in

      Breath.Result.reduce [
        action1
      ; sign_action1
      ; Breath.Expect.fail_with_message "You have already signed this proposal" sign_action2
      ])

let case_close_proposal_1_1 =
  Breath.Model.case
  "test close proposal with threshold 1 of 1"
  "successufully close proposal"
    (fun (level: Breath.Logger.level) ->
      let (_, (_alice, bob, _carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [bob.address;] in
      let init_storage = Helper.init_storage (owners, 1n) in
      let multisig_contract = Helper.originate level Mock_contract.multisig_main init_storage 100tez in
      let add_contract = Breath.Contract.originate level "add_contr" Mock_contract.add_main 1n 0tez in
      let param = ([] : (nat proposal_content) list) in

      let param1 = (Execute { target = add_contract.originated_address; parameter = 10n; amount = 10tez;} :: param) in
      let create_action1 = Breath.Context.act_as bob (Helper.create_proposal multisig_contract param1) in
      let pack1 = Helper.pack_proposal_content multisig_contract 1n in
      let sign_action1 = Breath.Context.act_as bob (Helper.sign_and_resolve_proposal multisig_contract 1n false pack1) in

      let balance = Breath.Contract.balance_of multisig_contract in
      let storage = Breath.Contract.storage_of multisig_contract in

      let proposal1 = Util.unopt (Big_map.find_opt 1n storage.proposals) "proposal 1 doesn't exist" in

      let add_contract_balance = Breath.Contract.balance_of add_contract in

      Breath.Result.reduce [
        create_action1
      ; sign_action1
      ; Breath.Assert.is_equal "balance" balance 100tez
      ; Breath.Assert.is_equal "balance of add contract" add_contract_balance 0tez
      ; Breath.Assert.is_equal "the counter of proposal" storage.proposal_counter 1n
      ; Assert.is_proposal_equal "#1 proposal" proposal1
        ({
          state            = Rejected;
          signatures       = Map.literal [(bob.address, false)];
          proposer         = { actor = bob.address; timestamp = Tezos.get_now () };
          resolver         = Some { actor = bob.address; timestamp = Tezos.get_now () };
          contents         = [ Execute {
            amount           = 10tez;
            target           = add_contract.originated_address;
            parameter        = 10n;
          }]
        })
      ])

let case_close_proposal_2_2 =
  Breath.Model.case
  "test close proposal with threshold 2 of 2"
  "successufully close proposal"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, _carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address;] in
      let init_storage = Helper.init_storage (owners, 2n) in
      let multisig_contract = Helper.originate level Mock_contract.multisig_main init_storage 100tez in
      let add_contract = Breath.Contract.originate level "add_contr" Mock_contract.add_main 1n 0tez in
      let param = ([] : (nat proposal_content) list) in

      let param1 = (Execute { target = add_contract.originated_address; parameter = 10n; amount = 10tez;} :: param) in
      let create_action1 = Breath.Context.act_as bob (Helper.create_proposal multisig_contract param1) in
      let pack1 = Helper.pack_proposal_content multisig_contract 1n in
      let sign_action1 = Breath.Context.act_as alice (Helper.sign_and_resolve_proposal multisig_contract 1n false pack1) in

      let balance = Breath.Contract.balance_of multisig_contract in
      let storage = Breath.Contract.storage_of multisig_contract in

      let proposal1 = Util.unopt (Big_map.find_opt 1n storage.proposals) "proposal 1 doesn't exist" in

      let add_contract_balance = Breath.Contract.balance_of add_contract in

      Breath.Result.reduce [
        create_action1
      ; sign_action1
      ; Breath.Assert.is_equal "balance" balance 100tez
      ; Breath.Assert.is_equal "balance of add contract" add_contract_balance 0tez
      ; Breath.Assert.is_equal "the counter of proposal" storage.proposal_counter 1n
      ; Assert.is_proposal_equal "#1 proposal" proposal1
        ({
          state            = Rejected;
          signatures       = Map.literal [(alice.address, false)];
          proposer         = { actor = bob.address; timestamp = Tezos.get_now () };
          resolver         = Some { actor = alice.address; timestamp = Tezos.get_now () };
          contents         = [ Execute {
            amount           = 10tez;
            target           = add_contract.originated_address;
            parameter        = 10n;
          }]
        })
      ])

let case_close_proposal_2_3 =
  Breath.Model.case
  "test close proposal with threshold 2 of 3"
  "successufully close proposal"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address; carol.address] in
      let init_storage = Helper.init_storage (owners, 2n) in
      let multisig_contract = Helper.originate level Mock_contract.multisig_main init_storage 100tez in
      let add_contract = Breath.Contract.originate level "add_contr" Mock_contract.add_main 1n 0tez in
      let param = ([] : (nat proposal_content) list) in

      let param1 = (Execute { target = add_contract.originated_address; parameter = 10n; amount = 10tez;} :: param) in
      let create_action1 = Breath.Context.act_as bob (Helper.create_proposal multisig_contract param1) in
      let pack1 = Helper.pack_proposal_content multisig_contract 1n in
      let sign_action1 = Breath.Context.act_as alice (Helper.sign_and_resolve_proposal multisig_contract 1n false pack1) in
      let sign_action2 = Breath.Context.act_as bob (Helper.sign_and_resolve_proposal multisig_contract 1n false pack1) in

      let balance = Breath.Contract.balance_of multisig_contract in
      let storage = Breath.Contract.storage_of multisig_contract in

      let proposal1 = Util.unopt (Big_map.find_opt 1n storage.proposals) "proposal 1 doesn't exist" in

      let add_contract_balance = Breath.Contract.balance_of add_contract in

      Breath.Result.reduce [
        create_action1
      ; sign_action1
      ; sign_action2
      ; Breath.Assert.is_equal "balance" balance 100tez
      ; Breath.Assert.is_equal "balance of add contract" add_contract_balance 0tez
      ; Breath.Assert.is_equal "the counter of proposal" storage.proposal_counter 1n
      ; Assert.is_proposal_equal "#1 proposal" proposal1
        ({
          state            = Rejected;
          signatures       = Map.literal [(bob.address, false); (alice.address, false)];
          proposer         = { actor = bob.address; timestamp = Tezos.get_now () };
          resolver         = Some { actor = bob.address; timestamp = Tezos.get_now () };
          contents         = [ Execute {
            amount           = 10tez;
            target           = add_contract.originated_address;
            parameter        = 10n;
          }]
        })
      ])

let case_not_closed_1_2 =
  Breath.Model.case
  "test not close proposal with threshold 1 of 2"
  "not close proposal"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address; carol.address] in
      let init_storage = Helper.init_storage (owners, 2n) in
      let multisig_contract = Helper.originate level Mock_contract.multisig_main init_storage 100tez in
      let add_contract = Breath.Contract.originate level "add_contr" Mock_contract.add_main 1n 0tez in
      let param = ([] : (nat proposal_content) list) in

      let param1 = (Execute { target = add_contract.originated_address; parameter = 10n; amount = 10tez;} :: param) in
      let create_action1 = Breath.Context.act_as bob (Helper.create_proposal multisig_contract param1) in
      let pack1 = Helper.pack_proposal_content multisig_contract 1n in
      let sign_action1 = Breath.Context.act_as alice (Helper.sign_and_resolve_proposal multisig_contract 1n false pack1) in

      let balance = Breath.Contract.balance_of multisig_contract in
      let storage = Breath.Contract.storage_of multisig_contract in

      let proposal1 = Util.unopt (Big_map.find_opt 1n storage.proposals) "proposal 1 doesn't exist" in

      let add_contract_balance = Breath.Contract.balance_of add_contract in

      Breath.Result.reduce [
        create_action1
      ; sign_action1
      ; Breath.Assert.is_equal "balance" balance 100tez
      ; Breath.Assert.is_equal "balance of add contract" add_contract_balance 0tez
      ; Breath.Assert.is_equal "the counter of proposal" storage.proposal_counter 1n
      ; Assert.is_proposal_equal "#1 proposal" proposal1
        ({
          state            = Proposing;
          signatures       = Map.literal [(alice.address, false)];
          proposer         = { actor = bob.address; timestamp = Tezos.get_now () };
          resolver         = None;
          contents         = [ Execute {
            amount           = 10tez;
            target           = add_contract.originated_address;
            parameter        = 10n;
          }]
        })
      ])

let test_suite =
  Breath.Model.suite "Suite for allowing disproposal" [
    case_sign_for_disapproval
  ; case_fail_double_sign
  ; case_close_proposal_1_1
  ; case_close_proposal_2_2
  ; case_close_proposal_2_3
  ; case_not_closed_1_2
  ]
