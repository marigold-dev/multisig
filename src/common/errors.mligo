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

let only_owner = "Only the contract owners can perform this operation"
let amount_must_be_zero_tez = "You must not send tez to the smart contract"
let no_proposal_exist = "No proposal exists for this counter"
let has_already_signed = "You have already signed this proposal"
let unknown_contract = "Unknown contract"
let invalidated_threshold = "Threshold must be greater than 1"
let no_owner = "No owner is set in the contract"
let no_enough_owner = "Number of owner should be greater than threshold"
let unresolved = "This proposal has been resolved"
let no_enough_signature_to_resolve = "No enough signature to resolve the proposal"
let no_proposal = "There is no content in proposal"
let no_owners = "No owner to be added or removed"
let amount_is_zero = "Amount should be greater than zero"
let not_the_same_content = "The bytes doesn't match the packed contents of proposal"
