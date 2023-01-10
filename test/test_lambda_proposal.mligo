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

let case_execute_lambda_proposal =
  Breath.Model.case
  "test create lambda proposal and execute"
  "successuful execute"
    (fun (level: Breath.Logger.level) ->
      let (_, (alice, bob, _carol)) = Breath.Context.init_default () in
      let owners : address set = Set.literal [alice.address; bob.address;] in
      let init_storage = Helper.init_storage (owners, 1n) in
      let multisig_contract = Helper.originate level Mock_contract.multisig_main init_storage 0tez in
      let add_contract = Breath.Contract.originate level "add_contr" Mock_contract.add_main 1n 0tez in
      let add_contract_address = add_contract.originated_address in

      let param = ([] : (nat proposal_content) list) in

      (* lambda *)
      let call_add_contract (_ : unit) : operation =
        let add_contr = Tezos.get_contract_with_error add_contract_address "add contract doesn't exist" in
           Tezos.transaction 10n 0tez add_contr
      in

      (* create proposal *)
      let param = Execute_lambda { metadata = None; lambda = call_add_contract } :: param in
      let action = Breath.Context.act_as alice (Helper.create_proposal multisig_contract param) in
      let pack1 = Bytes.pack param in
      let sign_action = Breath.Context.act_as bob (Helper.sign_and_resolve_proposal multisig_contract 1n true pack1) in

      let add_contract_storage = Breath.Contract.storage_of add_contract in

      Breath.Result.reduce [
        action
      ; sign_action
      ; Breath.Assert.is_equal "storage of add contract" add_contract_storage 11n
      ])

let test_suite =
  Breath.Model.suite "Suite for lambda proposal" [
    case_execute_lambda_proposal
  ]

