(* Wolfram Language package *)

BeginPackage["StripeFunctions`"];

camelCase::usage = "";
SFormatPaymenMethod::usage = "";
SFormatRefund::usage = "";
SFormatDispute::usage = "";
SFormatCharge::usage = "";
SFormatBankAccount::usage = "";
SFormatTransferReversal::usage = "";
SFormatTransfer::usage = "";
SFormatBalanceTransactionFee::usage = "";
SFormatBalanceTransaction::usage = "";

Begin["`Private`"];

(*(* Import Functions *)

(*Currencies*)
$StripeCodeToCurrency = StripeCurrencies`$StripeCodeToCurrency;
$StripeCurrencyToCode = StripeCurrencies`$StripeCurrencyToCode;
$StripeCurrencies = StripeCurrencies`$StripeCurrencies;
$StripeNoCent = StripeCurrencies`$StripeNoCent;
$StripeInterpreterCountryList = StripeCurrencies`$StripeInterpreterCountryList;
(*Currencies*)

(*Camel*)
$StripeShippingCamel = StripeCamel`$StripeShippingCamel;
$StripePaymentMethodCamel = StripeCamel`$StripePaymentMethodCamel;
$StripeRefundCamel = StripeCamel`$StripeRefundCamel;
$StripeDisputeCamel = StripeCamel`$StripeDisputeCamel;
$StripeDisputeEvidenceCamel = StripeCamel`$StripeDisputeEvidenceCamel;
$StripeEvidenceDetailsCamel = StripeCamel`$StripeEvidenceDetailsCamel;
$StripeChargeCamel = StripeCamel`$StripeChargeCamel;
$StripeBankAccountCamel = StripeCamel`$StripeBankAccountCamel;
$StripeTransferReversalCamel = StripeCamel`$StripeTransferReversalCamel;
$StripeTransferCamel = StripeCamel`$StripeTransferCamel;
$StripeBalanceTransactionFeeCamel = StripeCamel`$StripeBalanceTransactionFeeCamel;
$StripeBalanceTransactionCamel = StripeCamel`$StripeBalanceTransactionCamel;
(*Camel*)

(* Import Functions *)*)

(* AbsoluteTime[{1970, 1, 1}] = 2208988800 *)

camelCase[l_List, rest___]:=camelCase[#,rest]&/@l

camelCase[str_String, separators_:{"_"}]:=StringReplace[
 StringReplace[
  StringReplace[str, 
   Thread[separators -> " "]], {WordBoundary ~~ word_ :> 
    ToUpperCase[word]}], {"Id"~~WordBoundary->"ID",WhitespaceCharacter -> "","Url"~~WordBoundary->"URL","Urls"~~WordBoundary->"URLs"}]

SFormatPaymenMethod[data_]:=Module[{var},
	var = Association@data;
	var["exp_month"] = DateObject[{var["exp_year"],var["exp_month"]},DateFormat->{"MonthNameShort"," ","Year"}];
	var["country"] = Replace[var["country"],$StripeInterpreterCountryList,{0}];
	var["metadata"] = If[var["metadata"]==={}, <||>, Association@var["metadata"]];
	Replace[Normal@Delete[var,"exp_year"],$StripePaymentMethodCamel,{2}]]

SFormatRefund[data_]:=Module[{var, amount, currency, currcode},
	var = Association@data; amount=var["amount"]; currency=var["currency"]; currcode = currency/.$StripeCodeToCurrency;
	var["amount"] = If[MemberQ[$StripeNoCent,currency],Quantity[amount, currcode],
		NumberForm[N[#], {10, 2}, NumberPadding -> {"", "0"}, ExponentFunction -> (Null &)]&@Quantity[(amount/100),currcode]];
	var["created"] = DateObject[(2208988800 + #) + 3600*$TimeZone, "TimeZone" -> $TimeZone]&@var["created"];
	var["metadata"] = If[var["metadata"]==={}, <||>, Association@var["metadata"]];
	Replace[Normal@KeyDrop[var, "currency"],$StripeRefundCamel,{2}]]

SFormatDispute[data_]:=Module[{var, amount, currency, currcode, evidence},
	var = Association@data; amount = var["amount"]; currency = var["currency"]; currcode = currency/.$StripeCodeToCurrency; evidence = var["evidence"];
	var["amount"] = If[MemberQ[$StripeNoCent,currency],Quantity[amount, currcode],
		NumberForm[N[#], {10, 2}, NumberPadding -> {"", "0"}, ExponentFunction -> (Null &)]&@Quantity[(amount/100),currcode]];
	var["created"] = DateObject[(2208988800 + #) + 3600*$TimeZone, "TimeZone" -> $TimeZone]&@var["created"];
	var["evidence"] = Association@Replace[evidence,$StripeDisputeEvidenceCamel,{2}];
	var["evidence_details"] = Association@Replace[var["evidence_details"],$StripeEvidenceDetailsCamel,{2}];
	var[["evidence_details","DueBy"]] = If[IntegerQ[#],DateObject[(2208988800 + #) + 3600*$TimeZone, "TimeZone" -> $TimeZone],Missing["NotAvailable"]]&@var[["evidence_details","DueBy"]];
	var["balance_transactions"] = Association/@SFormatBalanceTransaction/@var["balance_transactions"];
	var["metadata"] = If[var["metadata"]==={}, <||>, Association@var["metadata"]];
	Replace[Normal@KeyDrop[var,"currency"], $StripeDisputeCamel,{2}]]

SFormatCharge[data_]:=Module[{var, amount, refunded, currency, currcode, fraud, dispute},
	var = Association@data; amount=var["amount"]; refunded=var["amount_refunded"]; currency=var["currency"]; currcode = currency/.$StripeCodeToCurrency; fraud=var["fraud_details"]; dispute=var["dispute"];
	var["refunds"] = Association/@(SFormatRefund/@Lookup[var["refunds"],"data"]);
	var["source"] = Association@SFormatPaymenMethod@var["source"];
	var["shipping"] = Replace[var["shipping"],Append[$StripeShippingCamel,{x___Rule}:><|x|>],{0,-1}];
	var["dispute"] = If[dispute===Null, <||>, Association@SFormatDispute@var["dispute"]];
	var["fraud_details"] = If[fraud==={}, <||>, Association@Replace[fraud,{"stripe_report"->"StripeReport","user_report"->"UserReport"},{2}]];
	var["amount"] = If[MemberQ[$StripeNoCent,currency],Quantity[amount, currcode],
		NumberForm[N[#], {10, 2}, NumberPadding -> {"", "0"}, ExponentFunction -> (Null &)]&@Quantity[(amount/100),currcode]];
	var["amount_refunded"] = If[MemberQ[$StripeNoCent,currency],Quantity[refunded, currcode],
		NumberForm[N[#], {10, 2}, NumberPadding -> {"", "0"}, ExponentFunction -> (Null &)]&@Quantity[(refunded/100),currcode]];
	var["created"] = DateObject[(2208988800 + #) + 3600*$TimeZone, "TimeZone" -> $TimeZone]&@var["created"];
	var["metadata"] = If[var["metadata"]==={}, <||>, Association@var["metadata"]];
	var["failure_code"] = If[StringQ@var["failure_code"],camelCase@var["failure_code"]];
	Replace[Normal@KeyDrop[var,"currency"],$StripeChargeCamel,{2}]]

SFormatBankAccount[data_]:=Module[{var},
	var = Association@data;
	var["country"] = Replace[var["country"],$StripeInterpreterCountryList,{0}];
	var["currency"] = ToUpperCase@var["currency"];
	var["metadata"] = If[var["metadata"]==={}, <||>, Association@var["metadata"]];
	Replace[Normal@var,$StripeBankAccountCamel,{2}]]

SFormatTransferReversal[data_]:=Module[{var, amount, currency, currcode},
	var = Association@data; amount=var["amount"]; currency=var["currency"]; currcode = currency/.$StripeCodeToCurrency;
	var["amount"] = If[MemberQ[$StripeNoCent,currency],Quantity[amount, currcode],
		NumberForm[N[#], {10, 2}, NumberPadding -> {"", "0"}, ExponentFunction -> (Null &)]&@Quantity[(amount/100),currcode]];
	var["created"] = DateObject[(2208988800 + #) + 3600*$TimeZone, "TimeZone" -> $TimeZone]&@var["created"];
	var["metadata"] = If[var["metadata"]==={}, <||>, Association@var["metadata"]];
	Replace[Normal@KeyDrop[var,"currency"],$StripeTransferReversalCamel,{2}]]

SFormatTransfer[data_]:=Module[{var, amount, reversed, currency, currcode},
	var = Association@data; amount=var["amount"]; reversed=var["amount_reversed"]; currency=currency/.$StripeCodeToCurrency; currcode = currency/.$StripeCodeToCurrency;
	var["metadata"] = If[var["metadata"]==={},<||>,Association@var["metadata"]];
	var["reversals"] = Association/@(SFormatTransferReversal/@Lookup[var["reversals"],"data"]);
	var["amount"] = If[MemberQ[$StripeNoCent,currency],Quantity[amount, currcode],
		NumberForm[N[#], {10, 2}, NumberPadding -> {"", "0"}, ExponentFunction -> (Null &)]&@Quantity[(amount/100),currcode]];
	var["amount_reversed"] = If[MemberQ[$StripeNoCent,currency],Quantity[reversed, currcode],
		NumberForm[N[#], {10, 2}, NumberPadding -> {"", "0"}, ExponentFunction -> (Null &)]&@Quantity[(reversed/100),currcode]];
	var["created"] = DateObject[(2208988800 + #) + 3600*$TimeZone, "TimeZone" -> $TimeZone]&@var["created"];
	var["date"] = If[IntegerQ[#],DateObject[(2208988800 + #) + 3600*$TimeZone, "TimeZone" -> $TimeZone],#]&@var["date"];
	var["metadata"] = If[var["metadata"]==={}, <||>, Association@var["metadata"]];
	Which[
			var["type"]==="card",
				var["card"] = Association@SFormatPaymenMethod@var["card"],

			var["type"]==="bank_account",
				var["bank_account"] = Association@SFormatBankAccount@var["bank_account"],

			var["type"]==="stripe_account",
				Null
	];
	Replace[Normal@KeyDrop[var,"currency"],$StripeTransferCamel,{2}]]

SFormatBalanceTransactionFee[data_]:=Module[{var, amount, currency, currcode},
	var = Association@data; amount=var["amount"]; currency=var["currency"]; currcode = currency/.$StripeCodeToCurrency;
	var["amount"] = If[MemberQ[$StripeNoCent,currency],Quantity[amount, currcode],
		NumberForm[N[#], {10, 2}, NumberPadding -> {"", "0"}, ExponentFunction -> (Null &)]&@Quantity[(amount/100),currcode]];
	Replace[Normal@KeyDrop[var,"currency"],$StripeBalanceTransactionFeeCamel,{2}]]

SFormatBalanceTransaction[data_]:=Module[{var, amount, net, fee, currency, currcode},
	var = Association@data; amount=var["amount"]; net=var["net"]; currency=var["currency"]; currcode = currency/.$StripeCodeToCurrency;; fee=var["fee"];
	var["amount"] = If[MemberQ[$StripeNoCent,currency],Quantity[amount, currcode],
		NumberForm[N[#], {10, 2}, NumberPadding -> {"", "0"}, ExponentFunction -> (Null &)]&@Quantity[(amount/100),currcode]];
	var["net"] = If[MemberQ[$StripeNoCent,currency],Quantity[net, currcode],
		NumberForm[N[#], {10, 2}, NumberPadding -> {"", "0"}, ExponentFunction -> (Null &)]&@Quantity[(net/100),currcode]];
	var["fee"] = If[MemberQ[$StripeNoCent,currency],Quantity[fee, currcode],
		NumberForm[N[#], {10, 2}, NumberPadding -> {"", "0"}, ExponentFunction -> (Null &)]&@Quantity[(fee/100),currcode]];
	var["fee_details"] = Association/@(SFormatBalanceTransactionFee/@var["fee_details"]);
	var["created"] = DateObject[(2208988800 + #) + 3600*$TimeZone, "TimeZone" -> $TimeZone]&@var["created"];
	var["sourced_transfers"] = Association/@(SFormatTransfer/@Lookup[var["sourced_transfers"],"data"]);
	var["available_on"] = If[IntegerQ[#],DateObject[(2208988800 + #) + 3600*$TimeZone, "TimeZone" -> $TimeZone],Missing["NotAvailable"]]&@var["available_on"];
	Replace[Normal@KeyDrop[var,"currency"],$StripeBalanceTransactionCamel,{2}]]

End[];
 
EndPackage[];