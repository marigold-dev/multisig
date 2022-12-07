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

let case_create_proposal =
  Breath.Model.case
  "test create proposal"
  "successuful create proposal"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, _carol)) = Breath.Context.init_default () in
      let signers : address set = Set.literal [alice.address; bob.address;] in
      let init_storage = Helper.init_storage (signers, 2n) in
      let multisig_contract = Helper.originate level Mock_contract.multisig_main init_storage 0tez in
      let add_contract = Breath.Contract.originate level "add_contr" Mock_contract.add_main 1n 0tez in

      (* create proposal 1 *)
      let param1 = (Raw_execute { target = add_contract.originated_address; parameter = 10n; amount = 0tez;}) in
      let action1 = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param1) in

      (* create proposal 2 *)
      let param2 = (Raw_execute { target = add_contract.originated_address; parameter = 20n; amount = 10tez;}) in
      let action2 = Breath.Context.act_as bob (Helper.create_proposal multisig_contract param2) in

      (* create proposal 2 *)
      let param3 = (Raw_transfer { target = alice.address; parameter = (); amount = 10tez;}) in
      let action3 = Breath.Context.act_as bob (Helper.create_proposal multisig_contract param3) in

      let balance = Breath.Contract.balance_of multisig_contract in
      let storage = Breath.Contract.storage_of multisig_contract in

      let proposal1 = Util.unopt (Big_map.find_opt 1n storage.proposal_map) "proposal 1 doesn't exist" in
      let proposal2 = Util.unopt (Big_map.find_opt 2n storage.proposal_map) "proposal 2 doesn't exist" in
      let proposal3 = Util.unopt (Big_map.find_opt 3n storage.proposal_map) "proposal 3 doesn't exist" in

      Breath.Result.reduce [
        action1
      ; action2
      ; action3
      ; Breath.Assert.is_equal "balance" balance 0tez
      ; Breath.Assert.is_equal "the counter of proposal" storage.proposal_counter 3n
      ; Assert.is_proposal_equal "#1 proposal" proposal1
        ( Execute {
          approved_signers = Set.empty;
          proposer         = alice.address;
          executed         = false;
          number_of_signer = 0n;
          target           = add_contract.originated_address;
          parameter        = 10n;
          amount           = 0tez;
          timestamp        = Tezos.get_now ();
        })
      ; Assert.is_proposal_equal "#2 proposal" proposal2
        ( Execute {
          approved_signers = Set.empty;
          proposer         = bob.address;
          executed         = false;
          number_of_signer = 0n;
          target           = add_contract.originated_address;
          parameter        = 20n;
          amount           = 10tez;
          timestamp        = Tezos.get_now ();
        })
      ; Assert.is_proposal_equal "#3 proposal" proposal3
        ( Transfer {
          approved_signers = Set.empty;
          proposer         = bob.address;
          executed         = false;
          number_of_signer = 0n;
          target           = alice.address;
          parameter        = ();
          amount           = 10tez;
          timestamp        = Tezos.get_now ();
        })
      ])

let case_unauthorized_user_fail_to_create_proposal =
  Breath.Model.case
  "unauthorized user creates proposal"
  "fail to create proposal"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, carol)) = Breath.Context.init_default () in
      let signers : address set = Set.literal [alice.address; bob.address;] in
      let init_storage = Helper.init_storage (signers, 2n) in
      let multisig_contract = Helper.originate level Mock_contract.multisig_main init_storage 0tez in
      let add_contract = Breath.Contract.originate level "add_contr" Mock_contract.add_main 1n 0tez in

      (* create proposal 1 *)
      let param1 = (Raw_execute { target = add_contract.originated_address; parameter = 10n; amount = 0tez;}) in
      let action1 = Breath.Context.act_as carol (Helper.create_proposal multisig_contract param1) in

      (* create proposal 1 *)
      let param2 = (Raw_transfer { target = alice.address; parameter = (); amount = 0tez;}) in
      let action2 = Breath.Context.act_as carol (Helper.create_proposal multisig_contract param2) in

      let balance = Breath.Contract.balance_of multisig_contract in
      let storage = Breath.Contract.storage_of multisig_contract in

      Breath.Result.reduce [
        Breath.Expect.fail_with_message "Only the contract signers can perform this operation" action1
      ; Breath.Expect.fail_with_message "Only the contract signers can perform this operation" action2
      ; Breath.Assert.is_equal "balance" balance 0tez
      ; Breath.Assert.is_equal "the counter of proposal" storage.proposal_counter 0n
      ])

let test_suite =
  Breath.Model.suite "Suite for create proposal" [
    case_create_proposal
  ; case_unauthorized_user_fail_to_create_proposal
  ]

