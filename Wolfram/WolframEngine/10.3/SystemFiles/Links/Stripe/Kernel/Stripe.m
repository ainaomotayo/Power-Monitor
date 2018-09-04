Begin["StripeOAuth`"] (* Begin Service Context *)

Needs["StripeCamel`"];
Needs["StripeFunctions`"];
Needs["StripeCurrencies`"];

(*****************************************Error handling***********************************************)
ServiceExecute::invid="`1` can't receive additional parameters after specifying a `2`."
ServiceExecute::crparam="Either the \"CustomerID\" or \"PaymentMethod\" is Required."
ServiceExecute::drparam="Use \"UpdateCustomer\" to create a new \"PaymentMethod\" for a customer."
ServiceExecute::mparam="The parameter `1` is required in \"PaymentMethod\"."
ServiceExecute::scsdel="Succesfully deleted the `1` with ID `2`."
(*****************************************Error handling***********************************************)

Begin["`Private`"](* Begin Private Context *)

 Import Functions 

(*Currencies*)
$StripeCurrencyToCode = StripeCurrencies`$StripeCurrencyToCode;
$StripeCurrencies = StripeCurrencies`$StripeCurrencies;
$StripeNoCent = StripeCurrencies`$StripeNoCent;
(*Currencies*)

(* Import Functions *)
(******************************* Stripe *************************************)

(* Authentication information *)

stripedata[]={
	"OAuthVersion"			-> "2.0",
	"ServiceName" 			-> "Stripe",
 	"AuthorizeEndpoint" 	-> "https://connect.stripe.com/oauth/authorize",
 	"AccessEndpoint"    	-> "https://connect.stripe.com/oauth/token",
 	"RedirectURI"       	-> "https://www.wolfram.com/oauthlanding?service=Stripe",
 	"ClientInfo"			-> {"ca_68xgg1YfSY7i4WioxN81CxZyM9mXiikW","sk_test_uENFr7pJaGcMOC2oirqb4Cwd"},
 	"AuthenticationDialog"	:> (OAuthClient`tokenOAuthDialog[#, "Stripe"]&),
 	"RequestFormat" 		-> (*{"Headers","Bearer"}*)(Block[{params=Lookup[{##2},"Parameters",{}],method=Lookup[{##2},"Method","GET"],body=Lookup[{##2},"BodyData",""],
							auth}, auth = Lookup[params,"access_token",""];
								Which[
									method === "GET",
									URLFetch[#1,{"StatusCode", "Content"},"Headers" -> {"Authorization" ->"bearer  "<>auth},"Method"->"GET","Parameters"->DeleteCases[params,("access_token" ->_)],"VerifyPeer" -> True],

									method === "POST",
									URLFetch[#1,{"StatusCode", "Content"},"Headers" -> {"Authorization" ->"bearer  "<>auth},"Method"->"POST","BodyData"->body,"VerifyPeer" -> True],

									method === "DELETE",
									URLFetch[#1,{"StatusCode", "Content"},"Headers" -> {"Authorization" ->"bearer  "<>auth},"Method"->"DELETE","Parameters"->DeleteCases[params,("access_token" ->_)],"VerifyPeer" -> True]
								]]&),
 	"Gets"					:> {"ListCharges","ListRefunds","ListCustomers","ListPaymentMethods","ListSubscriptions","ListPlans","ListBalance","ListBalanceTransaction","ListBalanceHistory"},
 	"Posts"					:> {"AddCharge","CaptureCharge","UpdateCharge","AddRefund","UpdateRefund","AddCustomer","UpdateCustomer","AddPaymentMethod","UpdatePaymentMethod","AddSubscription","UpdateSubscription","AddPlan","UpdatePlan"},
  	"Deletes"				:> {"RemoveCustomer","RemovePaymentMethod","RemoveSubscription","RemovePlan"},
 	"Scope"					-> {"read_write"},
 	"RawGets"				:> {"RawChargeRetrieve","RawChargeList","RawRefundRetrieve","RawRefundList","RawCustomerRetrieve","RawCustomerList","RawCardRetrieve","RawCardList","RawSubscriptionRetrieve","RawSubscriptionList","RawPlanRetrieve","RawPlanList","RawBalanceRetrieve","RawBalanceRetrieveTransaction","RawBalanceHistoryList"},
 	"RawPosts"				:> {"RawSourceChargeCreate","RawGetCardKey","RawCustomerChargeCreate","RawChargeUpdate","RawChargeCapture","RawRefundCreate","RawRefundUpdate","RawCustomerCreate","RawCustomerUpdate","RawCardCreate","RawCardUpdate","RawSubscriptionCreate","RawSubscriptionUpdate","RawPlanCreate","RawPlanUpdate"},
 	"RawDeletes"			:> {"RawCustomerDelete","RawCardDelete","RawSubscriptionDelete","RawPlanDelete"},
 	"Information"			-> "A service for managing stripe accounts"
}

(** Auxiliar functions **)
stripeimportdata[rawdata_]:=Module[{},
	If[rawdata=!="",ImportString[rawdata[[2]],"JSON"],Missing["NotAvailable"]]
	]

(** Raw functions **)
stripedata["RawGetCardKey"]:={
		"URL" 				-> "https://api.stripe.com/v1/tokens",
		"HTTPSMethod" 		-> "POST",
		"Parameters" 		-> {"card[number]","card[exp_year]","card[exp_month]","card[cvc]"},
		"RequiredParameters"-> {"card[number]","card[exp_year]","card[exp_month]"},
		"ResultsFunction" 	-> stripeimportdata
}

stripedata["RawSourceChargeCreate"]:={
		"URL" 				-> "https://api.stripe.com/v1/charges",
		"BodyData"			-> {"ParameterlessBodyData"},
		"HTTPSMethod" 		-> "POST",
		"Parameters" 		-> {},
		"RequiredParameters"-> {},
		"ResultsFunction" 	-> stripeimportdata
}

stripedata["RawCustomerChargeCreate"]:={
		"URL" 				-> "https://api.stripe.com/v1/charges",
		"BodyData"			-> {"ParameterlessBodyData"},
		"HTTPSMethod" 		-> "POST",
		"Parameters" 		-> {},
		"RequiredParameters"-> {},
		"ResultsFunction" 	-> stripeimportdata
} 

stripedata["RawChargeRetrieve"]:={
		"URL" 				-> (ToString@StringForm["https://api.stripe.com/v1/charges/`1`", #]&),
		"HTTPSMethod" 		-> "GET",
		"PathParameters"	-> {"id"},
		"Parameters" 		-> {},
		"RequiredParameters"-> {"id"},
		"ResultsFunction" 	-> stripeimportdata
}

stripedata["RawChargeUpdate"]:={
		"URL" 				-> (ToString@StringForm["https://api.stripe.com/v1/charges/`1`", #]&),
		"BodyData"			-> {"ParameterlessBodyData"},
		"HTTPSMethod" 		-> "POST",
		"PathParameters"	-> {"id"},
		"Parameters" 		-> {},
		"RequiredParameters"-> {"id"},
		"ResultsFunction" 	-> stripeimportdata
}

stripedata["RawChargeCapture"]:={
		"URL" 				-> (ToString@StringForm["https://api.stripe.com/v1/charges/`1`/capture", #]&),
		"HTTPSMethod" 		-> "POST",
		"PathParameters"	-> {"id"},
		"Parameters" 		-> {"amount","application_fee","receipt_email","statement_descriptor"},
		"RequiredParameters"-> {"id","amount"},
		"ResultsFunction" 	-> stripeimportdata
}

stripedata["RawChargeList"]:={
		"URL" 				-> "https://api.stripe.com/v1/charges",
		"HTTPSMethod" 		-> "GET",
		"Parameters" 		-> {"created[gte]","created[lte]","created[gt]","created[lt]","customer","ending_before","limit","starting_after"},
		"RequiredParameters"-> {},
		"ResultsFunction" 	-> stripeimportdata
}

stripedata["RawRefundCreate"]:={
		"URL" 				-> (ToString@StringForm["https://api.stripe.com/v1/charges/`1`/refunds", #]&),
		"BodyData"			-> {"ParameterlessBodyData"},
		"HTTPSMethod" 		-> "POST",
		"PathParameters"	-> {"id"},
		"Parameters" 		-> {},
		"RequiredParameters"-> {"id"},
		"ResultsFunction" 	-> stripeimportdata
}

stripedata["RawRefundRetrieve"]:={
		"URL" 				-> (ToString@StringForm["https://api.stripe.com/v1/charges/`1`/refunds/`2`",#1,#2]&),
		"HTTPSMethod" 		-> "GET",
		"PathParameters"	-> {"charge","id"},
		"Parameters" 		-> {},
		"RequiredParameters"-> {"charge","id"},
		"ResultsFunction" 	-> stripeimportdata
}

stripedata["RawRefundUpdate"]:={
		"URL" 				-> (ToString@StringForm["https://api.stripe.com/v1/charges/`1`/refunds/`2`",#1,#2]&),
		"BodyData"			-> {"ParameterlessBodyData"},
		"HTTPSMethod" 		-> "POST",
		"PathParameters"	-> {"charge","id"},
		"Parameters" 		-> {},
		"RequiredParameters"-> {"charge","id"},
		"ResultsFunction" 	-> stripeimportdata
}

stripedata["RawRefundList"]:={
		"URL" 				-> (ToString@StringForm["https://api.stripe.com/v1/charges/`1`/refunds", #]&),
		"HTTPSMethod" 		-> "GET",
		"PathParameters"	-> {"id"},
		"Parameters" 		-> {"ending_before","limit","starting_after"},
		"RequiredParameters"-> {"id"},
		"ResultsFunction" 	-> stripeimportdata
}

stripedata["RawCustomerCreate"]:={
		"URL" 				-> "https://api.stripe.com/v1/customers",
		"BodyData"			-> {"ParameterlessBodyData"},
		"HTTPSMethod" 		-> "POST",
		"Parameters" 		-> {},
		"RequiredParameters"-> {},
		"ResultsFunction" 	-> stripeimportdata
}

stripedata["RawCustomerRetrieve"]:={
		"URL" 				-> (ToString@StringForm["https://api.stripe.com/v1/customers/`1`", #]&),
		"HTTPSMethod" 		-> "GET",
		"PathParameters"	-> {"id"},
		"Parameters" 		-> {},
		"RequiredParameters"-> {"id"},
		"ResultsFunction" 	-> stripeimportdata
}

stripedata["RawCustomerUpdate"]:={
		"URL" 				-> (ToString@StringForm["https://api.stripe.com/v1/customers/`1`", #]&),
		"BodyData"			-> {"ParameterlessBodyData"},
		"HTTPSMethod" 		-> "POST",
		"PathParameters"	-> {"id"},
		"Parameters" 		-> {},
		"RequiredParameters"-> {"id"},
		"ResultsFunction" 	-> stripeimportdata
}

stripedata["RawCustomerDelete"]:={
		"URL" 				-> (ToString@StringForm["https://api.stripe.com/v1/customers/`1`", #]&),
		"HTTPSMethod" 		-> "DELETE",
		"PathParameters"	-> {"id"},
		"Parameters" 		-> {},
		"RequiredParameters"-> {"id"},
		"ResultsFunction" 	-> stripeimportdata
}

stripedata["RawCustomerList"]:={
		"URL" 				-> "https://api.stripe.com/v1/customers",
		"HTTPSMethod" 		-> "GET",
		"Parameters" 		-> {"created[gte]","created[lte]","created[gt]","created[lt]","ending_before","limit","starting_after"},
		"RequiredParameters"-> {},
		"ResultsFunction" 	-> stripeimportdata
}

stripedata["RawCardCreate"]:={
		"URL" 				-> (ToString@StringForm["https://api.stripe.com/v1/customers/`1`/sources", #]&),
		"HTTPSMethod" 		-> "POST",
		"PathParameters"	-> {"id"},
		"Parameters" 		-> {"source"},
		"RequiredParameters"-> {"id","source"},
		"ResultsFunction" 	-> stripeimportdata
}

stripedata["RawCardRetrieve"]:={
		"URL" 				-> (ToString@StringForm["https://api.stripe.com/v1/customers/`1`/cards/`2`",#1,#2]&),
		"HTTPSMethod" 		-> "GET",
		"PathParameters"	-> {"customer","id"},
		"Parameters" 		-> {},
		"RequiredParameters"-> {"customer","id"},
		"ResultsFunction" 	-> stripeimportdata
}

stripedata["RawCardUpdate"]:={
		"URL" 				-> (ToString@StringForm["https://api.stripe.com/v1/customers/`1`/cards/`2`",#1,#2]&),
		"BodyData"			-> {"ParameterlessBodyData"},
		"HTTPSMethod" 		-> "POST",
		"PathParameters"	-> {"customer","id"},
		"Parameters" 		-> {},
		"RequiredParameters"-> {"customer","id"},
		"ResultsFunction" 	-> stripeimportdata
}

stripedata["RawCardDelete"]:={
		"URL" 				-> (ToString@StringForm["https://api.stripe.com/v1/customers/`1`/cards/`2`",#1,#2]&),
		"HTTPSMethod" 		-> "DELETE",
		"PathParameters"	-> {"customer","id"},
		"Parameters" 		-> {},
		"RequiredParameters"-> {"customer","id"},
		"ResultsFunction" 	-> stripeimportdata
}

stripedata["RawCardList"]:={
		"URL" 				-> (ToString@StringForm["https://api.stripe.com/v1/customers/`1`/sources", #]&),
		"HTTPSMethod" 		-> "GET",
		"PathParameters"	-> {"id"},
		"Parameters" 		-> {"ending_before","limit","starting_after"},
		"RequiredParameters"-> {"id"},
		"ResultsFunction" 	-> stripeimportdata
}

stripedata["RawSubscriptionCreate"]:={
		"URL" 				-> (ToString@StringForm["https://api.stripe.com/v1/customers/`1`/subscriptions", #]&),
		"BodyData"			-> {"ParameterlessBodyData"},
		"HTTPSMethod" 		-> "POST",
		"PathParameters"	-> {"id"},
		"Parameters" 		-> {},
		"RequiredParameters"-> {"id"},
		"ResultsFunction" 	-> stripeimportdata
}

stripedata["RawSubscriptionRetrieve"]:={
		"URL" 				-> (ToString@StringForm["https://api.stripe.com/v1/customers/`1`/subscriptions/`2`",#1,#2]&),
		"HTTPSMethod" 		-> "GET",
		"PathParameters"	-> {"customer","id"},
		"Parameters" 		-> {},
		"RequiredParameters"-> {"customer","id"},
		"ResultsFunction" 	-> stripeimportdata
}

stripedata["RawSubscriptionUpdate"]:={
		"URL" 				-> (ToString@StringForm["https://api.stripe.com/v1/customers/`1`/subscriptions/`2`",#1,#2]&),
		"BodyData"			-> {"ParameterlessBodyData"},
		"HTTPSMethod" 		-> "POST",
		"PathParameters"	-> {"customer","id"},
		"Parameters" 		-> {},
		"RequiredParameters"-> {"customer","id"},
		"ResultsFunction" 	-> stripeimportdata
}

stripedata["RawSubscriptionDelete"]:={
		"URL" 				-> (ToString@StringForm["https://api.stripe.com/v1/customers/`1`/subscriptions/`2`",#1,#2]&),
		"HTTPSMethod" 		-> "DELETE",
		"PathParameters"	-> {"customer","id"},
		"Parameters" 		-> {"at_period_end"},
		"RequiredParameters"-> {"customer","id"},
		"ResultsFunction" 	-> stripeimportdata
}

stripedata["RawSubscriptionList"]:={
		"URL" 				-> (ToString@StringForm["https://api.stripe.com/v1/customers/`1`/subscriptions", #]&),
		"HTTPSMethod" 		-> "GET",
		"PathParameters"	-> {"id"},
		"Parameters" 		-> {"ending_before","limit","starting_after"},
		"RequiredParameters"-> {"id"},
		"ResultsFunction" 	-> stripeimportdata
}

stripedata["RawPlanCreate"]:={
		"URL" 				-> "https://api.stripe.com/v1/plans",
		"BodyData"			-> {"ParameterlessBodyData"},
		"HTTPSMethod" 		-> "POST",
		"PathParameters"	-> {},
		"Parameters" 		-> {},
		"RequiredParameters"-> {},
		"ResultsFunction" 	-> stripeimportdata
}

stripedata["RawPlanRetrieve"]:={
		"URL" 				-> (ToString@StringForm["https://api.stripe.com/v1/plans/`1`", #]&),
		"HTTPSMethod" 		-> "GET",
		"PathParameters"	-> {"id"},
		"Parameters" 		-> {},
		"RequiredParameters"-> {"id"},
		"ResultsFunction" 	-> stripeimportdata
}

stripedata["RawPlanUpdate"]:={
		"URL" 				-> (ToString@StringForm["https://api.stripe.com/v1/plans/`1`", #]&),
		"BodyData"			-> {"ParameterlessBodyData"},
		"HTTPSMethod" 		-> "POST",
		"PathParameters"	-> {"id"},
		"Parameters" 		-> {},
		"RequiredParameters"-> {"id"},
		"ResultsFunction" 	-> stripeimportdata
}

stripedata["RawPlanDelete"]:={
		"URL" 				-> (ToString@StringForm["https://api.stripe.com/v1/plans/`1`", #]&),
		"HTTPSMethod" 		-> "DELETE",
		"PathParameters"	-> {"id"},
		"Parameters" 		-> {},
		"RequiredParameters"-> {"id"},
		"ResultsFunction" 	-> stripeimportdata
}

stripedata["RawPlanList"]:={
		"URL" 				-> "https://api.stripe.com/v1/plans",
		"HTTPSMethod" 		-> "GET",
		"Parameters" 		-> {"created[gte]","created[lte]","created[gt]","created[lt]","ending_before","limit","starting_after"},
		"RequiredParameters"-> {},
		"ResultsFunction" 	-> stripeimportdata
}

stripedata["RawBalanceRetrieve"]:={
		"URL" 				-> "https://api.stripe.com/v1/balance",
		"HTTPSMethod" 		-> "GET",
		"PathParameters"	-> {},
		"Parameters" 		-> {},
		"RequiredParameters"-> {},
		"ResultsFunction" 	-> stripeimportdata
}

stripedata["RawBalanceRetrieveTransaction"]:={
		"URL" 				-> (ToString@StringForm["https://api.stripe.com/v1/balance/history/`1`", #]&),
		"HTTPSMethod" 		-> "GET",
		"PathParameters"	-> {"id"},
		"Parameters" 		-> {},
		"RequiredParameters"-> {"id"},
		"ResultsFunction" 	-> stripeimportdata
}

stripedata["RawBalanceHistoryList"]:={
		"URL" 				-> "https://api.stripe.com/v1/balance/history",
		"HTTPSMethod" 		-> "GET",
		"Parameters" 		-> {"available_on[gte]","available_on[lte]","created[gte]","created[lte]","currency","ending_before","limit","source","starting_after","transfer","type"},
		"RequiredParameters"-> {},
		"ResultsFunction" 	-> stripeimportdata
}

stripedata["RawCouponCreate"]:={
		"URL" 				-> "https://api.stripe.com/v1/coupons",
		"BodyData"			-> {"ParameterlessBodyData"},
		"HTTPSMethod" 		-> "POST",
		"Parameters" 		-> {},
		"RequiredParameters"-> {},
		"ResultsFunction" 	-> stripeimportdata
}

stripedata["RawCouponRetrieve"]:={
		"URL" 				-> (ToString@StringForm["https://api.stripe.com/v1/coupons/`1`", #]&),
		"HTTPSMethod" 		-> "GET",
		"PathParameters"	-> {"id"},
		"Parameters" 		-> {},
		"RequiredParameters"-> {"id"},
		"ResultsFunction" 	-> stripeimportdata
}

stripedata["RawCouponUpdate"]:={
		"URL" 				-> (ToString@StringForm["https://api.stripe.com/v1/coupons/`1`", #]&),
		"BodyData"			-> {"ParameterlessBodyData"},
		"HTTPSMethod" 		-> "POST",
		"PathParameters"	-> {"id"},
		"Parameters" 		-> {},
		"RequiredParameters"-> {"id"},
		"ResultsFunction" 	-> stripeimportdata
}

stripedata["RawCouponDelete"]:={
		"URL" 				-> (ToString@StringForm["https://api.stripe.com/v1/coupons/`1`", #]&),
		"HTTPSMethod" 		-> "DELETE",
		"PathParameters"	-> {"id"},
		"Parameters" 		-> {},
		"RequiredParameters"-> {"id"},
		"ResultsFunction" 	-> stripeimportdata
}

stripedata["RawCouponList"]:={
		"URL" 				-> "https://api.stripe.com/v1/coupons",
		"HTTPSMethod" 		-> "GET",
		"Parameters" 		-> {"created[gte]","created[lte]","created[gt]","created[lt]","ending_before","limit","starting_after"},
		"RequiredParameters"-> {},
		"ResultsFunction" 	-> stripeimportdata
}

stripedata["RawCustomerDiscountDelete"]:={
		"URL" 				-> (ToString@StringForm["https://api.stripe.com/v1/customers/`1`/discount", #]&),
		"HTTPSMethod" 		-> "DELETE",
		"PathParameters"	-> {"id"},
		"Parameters" 		-> {},
		"RequiredParameters"-> {"id"},
		"ResultsFunction" 	-> stripeimportdata
}

stripedata["RawSubscriptionDiscountDelete"]:={
		"URL" 				-> (ToString@StringForm["https://api.stripe.com/v1/customers/`1`/subscriptions/`2`/discount", #1,#2]&),
		"HTTPSMethod" 		-> "DELETE",
		"PathParameters"	-> {"customer","id"},
		"Parameters" 		-> {},
		"RequiredParameters"-> {"customer","id"},
		"ResultsFunction" 	-> stripeimportdata
}

stripedata["RawApplicationFeeRetrieve"]:={
		"URL" 				-> (ToString@StringForm["https://api.stripe.com/v1/application_fees/`1`", #]&),
		"HTTPSMethod" 		-> "GET",
		"PathParameters"	-> {"id"},
		"Parameters" 		-> {},
		"RequiredParameters"-> {"id"},
		"ResultsFunction" 	-> stripeimportdata
}

stripedata["RawApplicationFeeList"]:={
		"URL" 				-> "https://api.stripe.com/v1/application_fees",
		"HTTPSMethod" 		-> "GET",
		"Parameters" 		-> {"chargeid","created[gte]","created[lte]","created[gt]","created[lt]","ending_before","limit","starting_after"},
		"RequiredParameters"-> {},
		"ResultsFunction" 	-> stripeimportdata
}

stripedata["RawApplicationFeeRefundCreate"]:={
		"URL" 				-> (ToString@StringForm["https://api.stripe.com/v1/application_fees/`1`/refunds", #]&),
		"BodyData"			-> {"ParameterlessBodyData"},
		"HTTPSMethod" 		-> "POST",
		"Parameters" 		-> {},
		"RequiredParameters"-> {},
		"ResultsFunction" 	-> stripeimportdata
}

stripedata["RawApplicationFeeRefundRetrieve"]:={
		"URL" 				-> (ToString@StringForm["https://api.stripe.com/v1/application_fees/`1`/refunds/`2`}", #1,#2]&),
		"HTTPSMethod" 		-> "GET",
		"PathParameters"	-> {"id","transfer"},
		"Parameters" 		-> {},
		"RequiredParameters"-> {"id","transfer"},
		"ResultsFunction" 	-> stripeimportdata
}

stripedata["RawApplicationFeeRefundUpdate"]:={
		"URL" 				-> (ToString@StringForm["https://api.stripe.com/v1/application_fees/`1`/refunds/`2`}", #1,#2]&),
		"BodyData"			-> {"ParameterlessBodyData"},
		"HTTPSMethod" 		-> "POST",
		"PathParameters"	-> {},
		"Parameters" 		-> {},
		"RequiredParameters"-> {},
		"ResultsFunction" 	-> stripeimportdata
}

stripedata["RawApplicationFeeRefundList"]:={
		"URL" 				-> (ToString@StringForm["https://api.stripe.com/v1/application_fees/`1`/refunds", #]&),
		"HTTPSMethod" 		-> "GET",
		"PathParameters"	-> {"id"},
		"Parameters" 		-> {"ending_before","limit","starting_after"},
		"RequiredParameters"-> {},
		"ResultsFunction" 	-> stripeimportdata
}

(** Cooked functions **)

stripecookeddata[req_,id_,rules___Rule]:=stripecookeddata[req,id,{rules}]

stripecookeddata["ListCharges",id_, args_?(MatchQ[#,{___Rule}]&)] :=  Block[{invalidParameters, params={}, chargeid, customer, limit, created, raw, predata, data, errmsg},

	invalidParameters = Select[Keys[args],!MemberQ[{"ChargeID","StartDate","CustomerID","MaxItems"},#]&]; 
	
	If[Length[invalidParameters]>0,
		(
			Message[ServiceObject::noget,#,"Stripe"]&/@invalidParameters;
			Throw[$Failed]
		)];
		
	Which[
				KeyExistsQ[args,"ChargeID"] && !Fold[Or[#1, #2] &, (KeyExistsQ[args, #] &) /@ {"StartDate","CustomerID","MaxItems"}],
					chargeid = "ChargeID" /. args;
					If[!StringQ[chargeid],
						(
						Message[ServiceExecute::nval,"ChargeID","Stripe"];
						Throw[$Failed]
						)];
					raw = stripeimportdata@OAuthClient`rawoauthdata[id,"RawChargeRetrieve",{"id"->chargeid}];
					
					If[!KeyExistsQ[raw, "error"],
						(
							data = SFormatCharge@raw
						),
						errmsg = Lookup[Lookup[raw,"error"],"message"];
						Message[ServiceExecute::apierr,errmsg];
						Throw[$Failed]
						];

					Dataset@Association[Replace[data, {r : {___Rule} :> Association[r]}, -1]]
				
				,
				
				KeyExistsQ[args,"ChargeID"] && Fold[Or[#1, #2] &, (KeyExistsQ[args, #] &) /@ {"StartDate","CustomerID","MaxItems"}],
					Message[ServiceExecute::invid,"ListCharges","ChargeID"];
					Throw[$Failed]
				,
				
				Fold[Or[#1, #2] &, (KeyExistsQ[args, #] &) /@ {"StartDate","CustomerID","MaxItems"}],
					If[KeyExistsQ[args,"StartDate"],
						(
						created = "StartDate"/.args;
							If[!DateObjectQ[created],
								Message[ServiceExecute::nval,"StartDate","Stripe"];
								Throw[$Failed]
							];
						params = Append[params,Rule["created[gte]",ToString[Round@AbsoluteTime[created]-AbsoluteTime[{1970, 1, 1}]]]];
						)
					];
					
					If[KeyExistsQ[args,"CustomerID"],
						(
						customer = "CustomerID"/.args;
							If[!StringQ[customer],
								Message[ServiceExecute::nval,"CustomerID","Stripe"];
								Throw[$Failed]
							];
						params = Append[params,Rule["customer",customer]];
						)
					];
					
					If[KeyExistsQ[args,"MaxItems"],
						(
						limit = "MaxItems"/.args;
							If[!(IntegerQ[limit] && limit>0),
								Message[ServiceExecute::nval,"MaxItems","Stripe"];
								Throw[$Failed]
							];
						params = Append[params,Rule["limit",ToString[limit]]];
						)
					];
					
					raw = stripeimportdata@OAuthClient`rawoauthdata[id,"RawChargeList",params];
					
					If[!KeyExistsQ[raw, "error"],
						(
							data = SFormatCharge /@ Lookup[raw,"data"];
						),
						errmsg = Lookup[Lookup[raw,"error"],"message"];
						Message[ServiceExecute::apierr,errmsg];
						Throw[$Failed]
						];
						Dataset[Association[Replace[#, {r : {___Rule} :> Association[r]}, -1]] & /@ data]
						
				,

				True,
				raw = OAuthClient`rawoauthdata[id,"RawChargeList",{}];
				Which[
						raw[[1]] === 200,
							raw = stripeimportdata@raw;
							data = SFormatCharge /@ Lookup[raw,"data"];
							Dataset[Association[Replace[#, {r : {___Rule} :> Association[r]}, -1]] & /@ data],

						Quotient[raw[[1]], 100] === 4,
						Message[ServiceExecute::apierr,"message" /. ("error" /. stripeimportdata@raw)];
						Throw[$Failed],
					
						True,
							Message[ServiceExecute::serrormsg,"message" /. ("error" /. stripeimportdata@raw)];
							Throw[$Failed]
					]
			]
]


(* ::Section:: *)
(* Region Title *)
stripecookeddata["AddCharge",id_, args_?(MatchQ[#,{___Rule}]&)] :=  Block[{invalidParameters,params={},cparams={},customer,payment,number,expdate,expmon,expyear,cvc,cardtoken,capture,amount,money,currency,description,metadata,
												shipping,name,carrier,phone,tracking,address,line1,line2,city,country,state,zip,fee,destination,receipt,statement,body,raw,data,errmsg},

	invalidParameters = Select[Keys[args],!MemberQ[{"CustomerID","Amount","PaymentMethod","Description","Capture",MetaInformation,"ShippingInformation","ApplicationFee","Destination","ReceiptEmail","StatementText"},#]&];
	
	If[Length[invalidParameters]>0,
		(
			Message[ServiceObject::noget,#,"Stripe"]&/@invalidParameters;
			Throw[$Failed]
		)];
	
	Which[
				KeyExistsQ[args,"CustomerID"] && !KeyExistsQ[args,"PaymentMethod"],
					
					customer = "CustomerID"/.args;
					If[!StringQ[customer],
						(
						Message[ServiceExecute::nval,"Amount","Stripe"];
						Throw[$Failed]
						),
					params = Append[params,Rule["customer",customer]]
					];
					
					
					If[KeyExistsQ[args,"Amount"],
						(
						money = "Amount"/.args;
							If[!QuantityQ[money],
								Message[ServiceExecute::nval,"Amount","Stripe"];
								Throw[$Failed]
							];
						currency = QuantityUnit[money]/.$StripeCurrencyToCode;
						amount = QuantityMagnitude[money];
							If[!MemberQ[$StripeNoCent,currency],
							amount = 100*amount;
							];
							If[Or[!MemberQ[$StripeCurrencies,currency],!MatchQ[amount, x_ /; MemberQ[{Real, Integer}, Head[x]] && (x == Round[x]) && x>=0]],
								Message[ServiceExecute::nval,"Amount","Stripe"];
								Throw[$Failed]
							];
						amount = ToString@Round@amount;
						params = Append[params,Rule["amount",amount]];
						params = Append[params,Rule["currency",currency]];
						),
					Message[ServiceExecute::nparam,"Amount","Stripe"];
					Throw[$Failed]
					];
					
					If[KeyExistsQ[args,"Capture"],
						(
						capture = "Capture"/.args;
						If[!BooleanQ[capture],
								Message[ServiceExecute::nval,"Capture","Stripe"];
								Throw[$Failed]
							];
						params = Append[params,Rule["capture",ToLowerCase@ToString[capture]]];
						),
						params = Append[params,Rule["capture","true"]];
					];
					
					If[KeyExistsQ[args,"Description"],
						(
						description = "Description"/.args;
						If[!StringQ[description],
								Message[ServiceExecute::nval,"Description","Stripe"];
								Throw[$Failed]
							];
						params = Append[params,Rule["description",description]];
						)
					];
					
					If[KeyExistsQ[args,MetaInformation],
						(
							metadata = MetaInformation/.args;
							If[!Or[AssociationQ[metadata], Head[metadata] === List],
								Message[ServiceExecute::nval,MetaInformation,"Stripe"];
								Throw[$Failed]
							];
							params = Join[params, (Rule["metadata[" <> First[#] <> "]",Last[#]]) & /@ Normal@metadata];
						)
					];
					
					If[KeyExistsQ[args,"ShippingInformation"],
						(
							shipping = "ShippingInformation"/.args;
							If[!AssociationQ[shipping],
								Message[ServiceExecute::nval,"ShippingInformation","Stripe"];
								Throw[$Failed]
							];
							
							invalidParameters = Select[Keys[shipping],!MemberQ[{"Address","Name","Phone","Carrier","TrackingNumber"},#]&];
					
							If[Length[invalidParameters]>0,
								(
									Message[ServiceObject::noget,#,"Stripe"]&/@invalidParameters;
									Throw[$Failed]
								)];
								
							If[KeyExistsQ[shipping,"Name"],
								name = "Name"/.shipping;
								If[!StringQ[name],
									(
									Message[ServiceExecute::nval,"Name","Stripe"];
									Throw[$Failed]
									),
								params = Append[params,Rule["shipping[name]",name]]
								],
							Message[ServiceExecute::nparam,"Name","Stripe"];
							Throw[$Failed]
							];
							
							If[KeyExistsQ[shipping,"Address"],
								(
									address = "Address"/.shipping;
									
									If[!AssociationQ[address],
										Message[ServiceExecute::nval,"Address","Stripe"];
										Throw[$Failed]
									];

									invalidParameters = Select[Keys[address],!MemberQ[{"FirstLine","SecondLine","City","State","Country","PostalCode"},#]&];
					
									If[Length[invalidParameters]>0,
										(
											Message[ServiceObject::noget,#,"Stripe"]&/@invalidParameters;
											Throw[$Failed]
										)];

									If[KeyExistsQ[address,"FirstLine"],
										line1 = "FirstLine"/.address;
										If[!StringQ[line1],
											(
											Message[ServiceExecute::nval,"FirstLine","Stripe"];
											Throw[$Failed]
											),
										params = Append[params,Rule["shipping[address][line1]",line1]]
										],
									Message[ServiceExecute::nparam,"FirstLine","Stripe"];
									Throw[$Failed]
									];
						
									If[KeyExistsQ[address,"SecondLine"],
										line2 = "SecondLine"/.address;
										If[!StringQ[line2],
											(
											Message[ServiceExecute::nval,"SecondLine","Stripe"];
											Throw[$Failed]
											),
										params = Append[params,Rule["shipping[address][line2]",line2]]
										];
									];

									If[KeyExistsQ[address,"City"],
										city = "City"/.address;
										If[!StringQ[city],
											(
											Message[ServiceExecute::nval,"City","Stripe"];
											Throw[$Failed]
											),
										params = Append[params,Rule["shipping[address][city]",city]]
										];
									];
									
									If[KeyExistsQ[address,"Country"],
										country = "Country"/.address;
										If[!StringQ[country],
											(
											Message[ServiceExecute::nval,"Country","Stripe"];
											Throw[$Failed]
											),
										params = Append[params,Rule["shipping[address][country]",country]]
										];
									];
						
									If[KeyExistsQ[address,"State"],
										state = "State"/.address;
										If[!StringQ[state],
											(
											Message[ServiceExecute::nval,"State","Stripe"];
											Throw[$Failed]
											),
										params = Append[params,Rule["shipping[address][state]",state]]
										];
									];
						
									If[KeyExistsQ[address,"PostalCode"],
										zip = "PostalCode"/.address;
										If[!StringQ[zip],
											(
											Message[ServiceExecute::nval,"PostalCode","Stripe"];
											Throw[$Failed]
											),
										params = Append[params,Rule["shipping[address][postal_code]",zip]]
										];
									];
								)
							];
							
							If[KeyExistsQ[shipping,"Carrier"],
								carrier = "Carrier"/.shipping;
								If[!StringQ[carrier],
									(
									Message[ServiceExecute::nval,"Carrier","Stripe"];
									Throw[$Failed]
									),
								params = Append[params,Rule["shipping[carrier]",carrier]]
								]
							];
							
							If[KeyExistsQ[shipping,"Phone"],
								phone = "Phone"/.shipping;
								If[!StringQ[carrier],
									(
									Message[ServiceExecute::nval,"Phone","Stripe"];
									Throw[$Failed]
									),
								params = Append[params,Rule["shipping[phone]",phone]]
								]
							];
							
							If[KeyExistsQ[shipping,"TrackingNumber"],
								tracking = "TrackingNumber"/.shipping;
								If[!StringQ[tracking],
									(
									Message[ServiceExecute::nval,"TrackingNumber","Stripe"];
									Throw[$Failed]
									),
								params = Append[params,Rule["shipping[tracking_number]",tracking]]
								]
							];
						)
					];
					
					If[KeyExistsQ[args,"ApplicationFee"],
						(
						fee = "ApplicationFee"/.args;
						If[Or[!QuantityQ[fee],!((QuantityUnit[fee]/.$StripeCurrencyToCode)===currency),QuantityMagnitude[fee]<0],
								Message[ServiceExecute::nval,"ApplicationFee","Stripe"];
								Throw[$Failed]
							];
						If[!MemberQ[$StripeNoCent,currency],
							fee = ToString@(100*QuantityMagnitude[fee]),
							fee = ToString@QuantityMagnitude[fee]
							];
						params = Append[params,Rule["application_fee",fee]];
						)
					];
					
					(** NEEDS FURTHER TESTING TO VALIDATE ITS USAGE
					If[KeyExistsQ[args,"Destination"],
						(
						destination = "Destination"/.args;
						If[!StringQ[destination],
								Message[ServiceExecute::nval,"Destination","Stripe"];
								Throw[$Failed]
							];
						params = Append[params,Rule["destination",destination]];
						)
					];**)
					
					If[KeyExistsQ[args,"ReceiptEmail"],
						(
						receipt = "ReceiptEmail"/.args;
						If[!StringQ[receipt],
								Message[ServiceExecute::nval,"ReceiptEmail","Stripe"];
								Throw[$Failed]
							];
						params = Append[params,Rule["receipt_email",receipt]];
						)
					];
					
					If[KeyExistsQ[args,"StatementText"],
						(
						statement = "StatementText"/.args;
						If[!StringQ[statement],
								Message[ServiceExecute::nval,"StatementText","Stripe"];
								Throw[$Failed]
							];
						params = Append[params,Rule["statement_descriptor",statement]];
						)
					];

					body = Fold[#1 <> "&" <> #2 &, First[#] <> "=" <> Last[#] & /@ params];
				
					raw = stripeimportdata@OAuthClient`rawoauthdata[id,"RawCustomerChargeCreate",{"ParameterlessBodyData"->body}];
					
					If[!KeyExistsQ[raw, "error"],
						(
							data = SFormatCharge@raw
						),
						errmsg = Lookup[Lookup[raw,"error"],"message"];
						Message[ServiceExecute::apierr,errmsg];
						Throw[$Failed]
						];

					Dataset@Association[Replace[data, {r : {___Rule} :> Association[r]}, -1]]

					
				,
				
				KeyExistsQ[args,"PaymentMethod"] && !KeyExistsQ[args,"CustomerID"],
					
					payment = "PaymentMethod"/.args;
					
					If[!AssociationQ[payment],
							Message[ServiceExecute::nval,"PaymentMethod","Stripe"];
							Throw[$Failed]
					];
					
					invalidParameters = Select[Keys[payment],!MemberQ[{"Number","ExpirationDate","CVC"},#]&];
	
					If[Length[invalidParameters]>0,
						(
							Message[ServiceObject::noget,#,"Stripe"]&/@invalidParameters;
							Throw[$Failed]
						)];
					
					If[KeyExistsQ[payment,"Number"],
						(
					number = "Number"/.payment;
						If[!Or[StringQ[number],IntegerQ[number]],
								Message[ServiceExecute::nval,"Number","Stripe"];
								Throw[$Failed]
							];
					number = ToString@number;
					cparams = Append[cparams,Rule["card[number]",number]];
						),
					Message[ServiceExecute::mparam,"Number","Stripe"];
					Throw[$Failed]
					];
					
					If[KeyExistsQ[payment,"ExpirationDate"],
						(
					expdate = "ExpirationDate"/.payment;
						If[!DateObjectQ[expdate],
								Message[ServiceExecute::nval,"ExpirationDate","Stripe"];
								Throw[$Failed]
							];
					expyear = ToString@DateList[expdate][[1]];
					expmon = ToString@DateList[expdate][[2]];
					cparams = Append[cparams,Rule["card[exp_year]",expyear]];
					cparams = Append[cparams,Rule["card[exp_month]",expmon]];
						),
					Message[ServiceExecute::mparam,"ExpirationDate","Stripe"];
					Throw[$Failed]
					];
					
					If[KeyExistsQ[payment,"CVC"],
						(
					cvc = "CVC"/.payment;
						If[!Or[StringQ[cvc],IntegerQ[cvc]],
								Message[ServiceExecute::nval,"CVC","Stripe"];
								Throw[$Failed]
							];
					cvc = ToString@cvc;
					cparams = Append[cparams,Rule["card[cvc]",cvc]];
						)
					];
					
					If[KeyExistsQ[args,"Amount"],
						(
						money = "Amount"/.args;
							If[!QuantityQ[money],
								Message[ServiceExecute::nval,"Amount","Stripe"];
								Throw[$Failed]
							];
						currency = QuantityUnit[money]/.$StripeCurrencyToCode;
						amount = QuantityMagnitude[money];
							If[!MemberQ[$StripeNoCent,currency],
							amount = 100*amount;
							];
							If[Or[!MemberQ[$StripeCurrencies,currency],!MatchQ[amount, x_ /; MemberQ[{Real, Integer}, Head[x]] && (x == Round[x]) && x>=0]],
								Message[ServiceExecute::nval,"Amount","Stripe"];
								Throw[$Failed]
							];
						amount = ToString@Round@amount;
						params = Append[params,Rule["amount",amount]];
						params = Append[params,Rule["currency",currency]];
						),
					Message[ServiceExecute::nparam,"Amount","Stripe"];
					Throw[$Failed]
					];
					
					If[KeyExistsQ[args,"Capture"],
						(
						capture = "Capture"/.args;
						If[!BooleanQ[capture],
								Message[ServiceExecute::nval,"Capture","Stripe"];
								Throw[$Failed]
							];
						params = Append[params,Rule["capture",ToLowerCase@ToString[capture]]];
						),
						params = Append[params,Rule["capture","true"]];
					];
					
					If[KeyExistsQ[args,"Description"],
						(
						description = "Description"/.args;
						If[!StringQ[description],
								Message[ServiceExecute::nval,"Description","Stripe"];
								Throw[$Failed]
							];
						params = Append[params,Rule["description",description]];
						)
					];
					
					If[KeyExistsQ[args,MetaInformation],
						(
							metadata = MetaInformation/.args;
							If[!Or[AssociationQ[metadata], Head[metadata] === List],
								Message[ServiceExecute::nval,MetaInformation,"Stripe"];
								Throw[$Failed]
							];
							params = Join[params, (Rule["metadata[" <> First[#] <> "]",Last[#]]) & /@ Normal@metadata];
						)
					];
					
					If[KeyExistsQ[args,"ShippingInformation"],
						(
							shipping = "ShippingInformation"/.args;
							If[!AssociationQ[shipping],
								Message[ServiceExecute::nval,"ShippingInformation","Stripe"];
								Throw[$Failed]
							];
							
							invalidParameters = Select[Keys[shipping],!MemberQ[{"Address","Name","Phone","Carrier","TrackingNumber"},#]&];
					
							If[Length[invalidParameters]>0,
								(
									Message[ServiceObject::noget,#,"Stripe"]&/@invalidParameters;
									Throw[$Failed]
								)];
								
							If[KeyExistsQ[shipping,"Name"],
								name = "Name"/.shipping;
								If[!StringQ[name],
									(
									Message[ServiceExecute::nval,"Name","Stripe"];
									Throw[$Failed]
									),
								params = Append[params,Rule["shipping[name]",name]]
								],
							Message[ServiceExecute::nparam,"Name","Stripe"];
							Throw[$Failed]
							];
							
							If[KeyExistsQ[shipping,"Address"],
								(
									address = "Address"/.shipping;
									
									If[!AssociationQ[address],
										Message[ServiceExecute::nval,"Address","Stripe"];
										Throw[$Failed]
									];

									invalidParameters = Select[Keys[address],!MemberQ[{"FirstLine","SecondLine","City","State","Country","PostalCode"},#]&];
					
									If[Length[invalidParameters]>0,
										(
											Message[ServiceObject::noget,#,"Stripe"]&/@invalidParameters;
											Throw[$Failed]
										)];

									If[KeyExistsQ[address,"FirstLine"],
										line1 = "FirstLine"/.address;
										If[!StringQ[line1],
											(
											Message[ServiceExecute::nval,"FirstLine","Stripe"];
											Throw[$Failed]
											),
										params = Append[params,Rule["shipping[address][line1]",line1]]
										],
									Message[ServiceExecute::nparam,"FirstLine","Stripe"];
									Throw[$Failed]
									];

									If[KeyExistsQ[address,"SecondLine"],
										line2 = "SecondLine"/.address;
										If[!StringQ[line2],
											(
											Message[ServiceExecute::nval,"SecondLine","Stripe"];
											Throw[$Failed]
											),
										params = Append[params,Rule["shipping[address][line2]",line2]]
										];
									];

									If[KeyExistsQ[address,"City"],
										city = "City"/.address;
										If[!StringQ[city],
											(
											Message[ServiceExecute::nval,"City","Stripe"];
											Throw[$Failed]
											),
										params = Append[params,Rule["shipping[address][city]",city]]
										];
									];

									If[KeyExistsQ[address,"Country"],
										country = "Country"/.address;
										If[!StringQ[country],
											(
											Message[ServiceExecute::nval,"Country","Stripe"];
											Throw[$Failed]
											),
										params = Append[params,Rule["shipping[address][country]",country]]
										];
									];

									If[KeyExistsQ[address,"State"],
										state = "State"/.address;
										If[!StringQ[state],
											(
											Message[ServiceExecute::nval,"State","Stripe"];
											Throw[$Failed]
											),
										params = Append[params,Rule["shipping[address][state]",state]]
										];
									];

									If[KeyExistsQ[address,"PostalCode"],
										zip = "PostalCode"/.address;
										If[!StringQ[zip],
											(
											Message[ServiceExecute::nval,"PostalCode","Stripe"];
											Throw[$Failed]
											),
										params = Append[params,Rule["shipping[address][postal_code]",zip]]
										];
									];
								)
							];

							If[KeyExistsQ[shipping,"Carrier"],
								carrier = "Carrier"/.shipping;
								If[!StringQ[carrier],
									(
									Message[ServiceExecute::nval,"Carrier","Stripe"];
									Throw[$Failed]
									),
								params = Append[params,Rule["shipping[carrier]",carrier]]
								]
							];
							
							If[KeyExistsQ[shipping,"Phone"],
								phone = "Phone"/.shipping;
								If[!StringQ[carrier],
									(
									Message[ServiceExecute::nval,"Phone","Stripe"];
									Throw[$Failed]
									),
								params = Append[params,Rule["shipping[phone]",phone]]
								]
							];
							
							If[KeyExistsQ[shipping,"TrackingNumber"],
								tracking = "TrackingNumber"/.shipping;
								If[!StringQ[tracking],
									(
									Message[ServiceExecute::nval,"TrackingNumber","Stripe"];
									Throw[$Failed]
									),
								params = Append[params,Rule["shipping[tracking_number]",tracking]]
								]
							];
						)
					];

					If[KeyExistsQ[args,"ApplicationFee"],
						(
						fee = "ApplicationFee"/.args;
						If[Or[!QuantityQ[fee],!((QuantityUnit[fee]/.$StripeCurrencyToCode)===currency),QuantityMagnitude[fee]<0],
								Message[ServiceExecute::nval,"ApplicationFee","Stripe"];
								Throw[$Failed]
							];
						If[!MemberQ[$StripeNoCent,currency],
							fee = ToString@(100*QuantityMagnitude[fee]),
							fee = ToString@QuantityMagnitude[fee]
							];
						params = Append[params,Rule["application_fee",fee]];
						)
					];

					(** NEEDS FURTHER TESTING TO VALIDATE ITS USAGE
					If[KeyExistsQ[args,"Destination"],
						(
						destination = "Destination"/.args;
						If[!StringQ[shipping],
								Message[ServiceExecute::nval,"Destination","Stripe"];
								Throw[$Failed]
							];
						params = Append[params,Rule["destination",destination]];
						)
					];**)
					
					If[KeyExistsQ[args,"ReceiptEmail"],
						(
						receipt = "ReceiptEmail"/.args;
						If[!StringQ[receipt],
								Message[ServiceExecute::nval,"ReceiptEmail","Stripe"];
								Throw[$Failed]
							];
						params = Append[params,Rule["receipt_email",receipt]];
						)
					];
					
					If[KeyExistsQ[args,"StatementText"],
						(
						statement = "StatementText"/.args;
						If[!StringQ[statement],
								Message[ServiceExecute::nval,"StatementText","Stripe"];
								Throw[$Failed]
							];
						params = Append[params,Rule["statement_descriptor",statement]];
						)
					];

					cardtoken = stripeimportdata@OAuthClient`rawoauthdata[id,"RawGetCardKey",cparams];
					
					If[KeyExistsQ[cardtoken, "error"],
						errmsg = "message" /. ("error" /. cardtoken);
							Message[ServiceExecute::apierr,errmsg];
							Throw[$Failed]
						];
					params = Append[params,Rule["source","id"/.cardtoken]];

					body = Fold[#1 <> "&" <> #2 &, First[#] <> "=" <> Last[#] & /@ params];

					raw = stripeimportdata@OAuthClient`rawoauthdata[id,"RawSourceChargeCreate",{"ParameterlessBodyData"->body}];
					If[!KeyExistsQ[raw, "error"],
						(
							data = SFormatCharge@raw
						),
						errmsg = Lookup[Lookup[raw,"error"],"message"];
						Message[ServiceExecute::apierr,errmsg];
						Throw[$Failed]
						];

					Dataset@Association[Replace[data, {r : {___Rule} :> Association[r]}, -1]]

				,
				
				KeyExistsQ[args,"PaymentMethod"] && KeyExistsQ[args,"CustomerID"],
				Message[ServiceExecute::drparam];
				Throw[$Failed]
				,

				True,
				Message[ServiceExecute::crparam];
				Throw[$Failed]
	]
]

stripecookeddata["CaptureCharge",id_, args_?(MatchQ[#,{___Rule}]&)] :=  Block[{invalidParameters,params={},chargeid,amount,currency,money,fee,receipt,statement,raw,data,errmsg},

	invalidParameters = Select[Keys[args],!MemberQ[{"ChargeID","Amount","ApplicationFee","ReceiptEmail","StatementText"},#]&]; 
	
	If[Length[invalidParameters]>0,
		(
			Message[ServiceObject::noget,#,"Stripe"]&/@invalidParameters;
			Throw[$Failed]
		)];	
		
	If[KeyExistsQ[args,"ChargeID"],
		(
			chargeid = "ChargeID" /. args;
			If[!StringQ[chargeid],
				(
				Message[ServiceExecute::nval,"ChargeID","Stripe"];
				Throw[$Failed]
				),
			params = Append[params,Rule["id",chargeid]]
			];
		),
		Message[ServiceExecute::nparam,"ChargeID","Stripe"];
		Throw[$Failed]
	];

	If[KeyExistsQ[args,"Amount"],
		(
		money = "Amount"/.args;
			If[!QuantityQ[money],
				Message[ServiceExecute::nval,"Amount","Stripe"];
				Throw[$Failed]
			];
		currency = QuantityUnit[money]/.$StripeCurrencyToCode;
		amount = QuantityMagnitude[money];
			If[!MemberQ[$StripeNoCent,currency],
				amount = 100*amount;
			];
			If[Or[!MemberQ[$StripeCurrencies,currency],!MatchQ[amount, x_ /; MemberQ[{Real, Integer}, Head[x]] && (x == Round[x]) && x>=0]],
				Message[ServiceExecute::nval,"Amount","Stripe"];
				Throw[$Failed]
			];	
		amount = ToString@Round@amount;
		params = Append[params,Rule["amount",amount]];
		),
		Message[ServiceExecute::nparam,"Amount","Stripe"];
		Throw[$Failed]	
	];
	
	If[KeyExistsQ[args,"ApplicationFee"],
		(
		fee = "ApplicationFee"/.args;
			If[Or[!QuantityQ[fee],!((QuantityUnit[fee]/.$StripeCurrencyToCode)===currency),QuantityMagnitude[fee]<0],
				Message[ServiceExecute::nval,"ApplicationFee","Stripe"];
				Throw[$Failed]
			];
		If[!MemberQ[$StripeNoCent,currency],
			fee = ToString@(100*QuantityMagnitude[fee]),
			fee = ToString@QuantityMagnitude[fee]
		];
		params = Append[params,Rule["application_fee",fee]];
		)
	];
	
	If[KeyExistsQ[args,"ReceiptEmail"],
	receipt = "ReceiptEmail" /. args;
		If[!StringQ[receipt],
			(
			Message[ServiceExecute::nval,"ReceiptEmail","Stripe"];
			Throw[$Failed]
			),
		params = Append[params,Rule["receipt_email",receipt]];
		]
	];
	
	If[KeyExistsQ[args,"StatementText"],
		(
		statement = "StatementText"/.args;
		If[!StringQ[statement],
			Message[ServiceExecute::nval,"StatementText","Stripe"];
			Throw[$Failed]
		];
		params = Append[params,Rule["statement_descriptor",statement]];
		)
	];

	raw = stripeimportdata@OAuthClient`rawoauthdata[id,"RawChargeCapture",params];
	
	If[!KeyExistsQ[raw, "error"],
		(
			data = SFormatCharge@raw
		),
		errmsg = Lookup[Lookup[raw,"error"],"message"];
		Message[ServiceExecute::apierr,errmsg];
		Throw[$Failed]
		];

	Dataset@Association[Replace[data, {r : {___Rule} :> Association[r]}, -1]]
	
]

stripecookeddata["UpdateCharge",id_, args_?(MatchQ[#,{___Rule}]&)] :=  Block[{invalidParameters,params={},chargeid,description,metadata,receipt,fraud,report,body,raw,predata,data,errmsg},

	invalidParameters = Select[Keys[args],!MemberQ[{"ChargeID","Description",MetaInformation,"ReceiptEmail","Fraud"},#]&]; 
	
	If[Length[invalidParameters]>0,
		(
			Message[ServiceObject::noget,#,"Stripe"]&/@invalidParameters;
			Throw[$Failed]
		)];
	
	If[KeyExistsQ[args,"ChargeID"],
		(
			chargeid = "ChargeID" /. args;
			If[!StringQ[chargeid],
				(
				Message[ServiceExecute::nval,"ChargeID","Stripe"];
				Throw[$Failed]
				)(**,
			params = Append[params,Rule["id",chargeid]]**)
			];
		),
		Message[ServiceExecute::nparam,"ChargeID","Stripe"];
		Throw[$Failed]
	];
	
	If[KeyExistsQ[args,"Description"],
		description = "Description" /. args;
		If[!StringQ[description],
			(
			Message[ServiceExecute::nval,"Description","Stripe"];
			Throw[$Failed]
			),
		params = Append[params,Rule["description",description]];
		]
	];


	If[KeyExistsQ[args,MetaInformation],
		(
			metadata = MetaInformation/.args;
			If[!Or[AssociationQ[metadata], Head[metadata] === List],
				Message[ServiceExecute::nval,MetaInformation,"Stripe"];
				Throw[$Failed]
			];
			params = Join[params, (Rule["metadata[" <> First[#] <> "]",Last[#]]) & /@ Normal@metadata];
		)
	];

	
	If[KeyExistsQ[args,"ReceiptEmail"],
	receipt = "ReceiptEmail" /. args;
		If[!StringQ[receipt],
			(
			Message[ServiceExecute::nval,"ReceiptEmail","Stripe"];
			Throw[$Failed]
			),
		params = Append[params,Rule["receipt_email",receipt]];
		]
	];
	
	If[KeyExistsQ[args,"Fraud"],
		(
			fraud = "Fraud"/.args;
			Which[
					AssociationQ[fraud],
					
					invalidParameters = Select[Keys[fraud],!MemberQ[{"ReportUser"},#]&]; 
	
					If[Length[invalidParameters]>0,
						(
							Message[ServiceObject::noget,#,"Stripe"]&/@invalidParameters;
							Throw[$Failed]
						)];					
					
					If[KeyExistsQ[fraud,"ReportUser"],
						report = "ReportUser"/.fraud;
						If[!BooleanQ[report],
							(
							Message[ServiceExecute::nval,"ReportUser","Stripe"];
							Throw[$Failed]
							),
						params = Append[params,Rule["fraud_details[user_report]",If[report,"fraudulent","safe"]]]
						];	
					],
					
					BooleanQ[fraud],
					params = Append[params,Rule["fraud_details[user_report]",If[fraud,"fraudulent","safe"]]],
					
					True,
					Message[ServiceExecute::nval,"Fraud","Stripe"];
					Throw[$Failed]
				]
		)
	];

	If[!(params === {}), body = Fold[#1 <> "&" <> #2 &, First[#] <> "=" <> Last[#] & /@ params], body = ""];

	raw = stripeimportdata@OAuthClient`rawoauthdata[id,"RawChargeUpdate",{"id"->chargeid,"ParameterlessBodyData"->body}];
	
	If[!KeyExistsQ[raw, "error"],
		(
			data = SFormatCharge@raw
		),
		errmsg = Lookup[Lookup[raw,"error"],"message"];
		Message[ServiceExecute::apierr,errmsg];
		Throw[$Failed]
		];

	Dataset@Association[Replace[data, {r : {___Rule} :> Association[r]}, -1]]
		
]

stripecookeddata["ListRefunds",id_, args_?(MatchQ[#,{___Rule}]&)] :=  Block[{invalidParameters,params={},chargeid,refundid,limit,raw,predata,data,errmsg},

	invalidParameters = Select[Keys[args],!MemberQ[{"ChargeID","RefundID","MaxItems"},#]&]; 
	
	If[Length[invalidParameters]>0,
		(
			Message[ServiceObject::noget,#,"Stripe"]&/@invalidParameters;
			Throw[$Failed]
		)];
		
	If[KeyExistsQ[args,"ChargeID"],
		(
		chargeid = "ChargeID" /. args;
		If[!StringQ[chargeid],
			(
			Message[ServiceExecute::nval,"ChargeID","Stripe"];
			Throw[$Failed]
			)];
		),
		Message[ServiceExecute::nparam,"ChargeID","Stripe"];
		Throw[$Failed]
	];
		
	Which[
				KeyExistsQ[args,"RefundID"] && !KeyExistsQ[args,"MaxItems"],
					refundid = "RefundID" /. args;
					If[!StringQ[refundid],
						(
						Message[ServiceExecute::nval,"RefundID","Stripe"];
						Throw[$Failed]
					),
					params = Append[params,Rule["id",refundid]];
					params = Append[params,Rule["charge",chargeid]]
					];
					
					raw = stripeimportdata@OAuthClient`rawoauthdata[id,"RawRefundRetrieve",params];
					If[!KeyExistsQ[#, "error"],
						(
							predata = Module[{quant, unix, dateObj},
								quant = Replace[#, List[a___, "amount" -> x_, b___, "currency" -> y_, c___] :> List[a, b, Rule["amount", If[MemberQ[$StripeNoCent, y], Quantity[x, y],NumberForm[N[#], {10, 2}, NumberPadding -> {"", "0"}, ExponentFunction -> (Null &)]&@Quantity[(x/100), y]]], c], {0}];
								unix = quant //. Rule["created", x_Integer] :> DateObject[AbsoluteTime[{1970, 1, 1}] + x, "TimeZone" -> 0];
								dateObj = unix //. List[a___, x_DateObject, b___] :> List[a, Rule["created", x], b]]
						),
						errmsg = "message" /. ("error" /. raw);
						Message[ServiceExecute::apierr,errmsg];
						Throw[$Failed]
						]&@ raw;
					predata = Replace[predata, x_Rule /; First[x] === "metadata" :> Rule[First[x], placeholder[First[#]] -> Last[#] & /@ Last[x]], Infinity];
					predata = Replace[predata, {Rule[string_String, x_] :> Rule[OAuthClient`Private`camelCase@string, x]}, Infinity];
					data = Replace[predata, x_Rule /; Head@First[x] === placeholder :> Rule[First[First[x]], Last[x]], Infinity];
					data = data //. {Rule["Reason", x_String] :> Rule["Reason", OAuthClient`Private`camelCase@x], Rule["Object", x_String] :> Rule["Object", OAuthClient`Private`camelCase@x],"Metadata":>MetaInformation};
					data = Replace[data,{Rule[rule_,Null]:>Rule[rule,Missing["NotAvailable"]]},Infinity];
					Dataset@Association[Replace[data, {r : {___Rule} :> Association[r]}, -1]]

				,
					
				KeyExistsQ[args,"RefundID"] && KeyExistsQ[args,"MaxItems"],
					Message[ServiceExecute::invid,"ListRefunds","ChargeID and RefundID"];
					Throw[$Failed]	
					
				,
				
				!KeyExistsQ[args,"RefundID"] && KeyExistsQ[args,"MaxItems"],
					params = Append[params,Rule["id",chargeid]];
					limit = "MaxItems" /. args;
					If[!(IntegerQ[limit] && limit>0),
						(
							Message[ServiceExecute::nval,"MaxItems","Stripe"];
							Throw[$Failed]
						),
						params = Append[params,Rule["limit",ToString[limit]]];
						];
					
					raw = stripeimportdata@OAuthClient`rawoauthdata[id,"RawRefundList",params];
					If[KeyExistsQ[raw, "error"],
						errmsg = "message" /. ("error" /. raw);
						Message[ServiceExecute::apierr,errmsg];
						Throw[$Failed],
						(
							predata = Module[{quant, unix, dateObj},
								quant = Replace[#, List[a___, "amount" -> x_, b___, "currency" -> y_, c___] :> List[a, b, Rule["amount", If[MemberQ[$StripeNoCent, y], Quantity[x, y],NumberForm[N[#], {10, 2}, NumberPadding -> {"", "0"}, ExponentFunction -> (Null &)]&@Quantity[(x/100), y]]], c], {0}];
								unix = quant //. Rule["created", x_Integer] :> DateObject[AbsoluteTime[{1970, 1, 1}] + x, "TimeZone" -> 0];
								dateObj = unix //. List[a___, x_DateObject, b___] :> List[a, Rule["created", x], b]
								]& /@ ("data" /.raw);
					
						predata = Replace[predata, x_Rule /; First[x] === "metadata" :> Rule[First[x], placeholder[First[#]] -> Last[#] & /@ Last[x]], Infinity];
						predata = Replace[predata, {Rule[string_String, x_] :> Rule[OAuthClient`Private`camelCase@string, x]}, Infinity];
						data = Replace[predata, x_Rule /; Head@First[x] === placeholder :> Rule[First[First[x]], Last[x]], Infinity];
						data = data //. {Rule["Reason", x_String] :> Rule["Reason", OAuthClient`Private`camelCase@x], Rule["Object", x_String] :> Rule["Object", OAuthClient`Private`camelCase@x],"Metadata":>MetaInformation};
						data = Replace[data,{Rule[rule_,Null]:>Rule[rule,Missing["NotAvailable"]]},Infinity];
						Dataset@(Association[Replace[#, {r : {___Rule} :> Association[r]}, -1]] & /@ data)
						)]
					
				,
				
				True,
					raw = stripeimportdata@OAuthClient`rawoauthdata[id,"RawRefundList",{"id"->chargeid}];
					If[KeyExistsQ[raw, "error"],
						errmsg = "message" /. ("error" /. raw);
						Message[ServiceExecute::apierr,errmsg];
						Throw[$Failed],
						(
							predata = Module[{quant, unix, dateObj},
								quant = Replace[#, List[a___, "amount" -> x_, b___, "currency" -> y_, c___] :> List[a, b, Rule["amount", If[MemberQ[$StripeNoCent, y], Quantity[x, y],NumberForm[N[#], {10, 2}, NumberPadding -> {"", "0"}, ExponentFunction -> (Null &)]&@Quantity[(x/100), y]]], c], {0}];
								unix = quant //. Rule["created", x_Integer] :> DateObject[AbsoluteTime[{1970, 1, 1}] + x, "TimeZone" -> 0];
								dateObj = unix //. List[a___, x_DateObject, b___] :> List[a, Rule["created", x], b]
								]& /@ ("data" /.raw);
					
						predata = Replace[predata, x_Rule /; First[x] === "metadata" :> Rule[First[x], placeholder[First[#]] -> Last[#] & /@ Last[x]], Infinity];
						predata = Replace[predata, {Rule[string_String, x_] :> Rule[OAuthClient`Private`camelCase@string, x]}, Infinity];
						data = Replace[predata, x_Rule /; Head@First[x] === placeholder :> Rule[First[First[x]], Last[x]], Infinity];
						data = data //. {Rule["Reason", x_String] :> Rule["Reason", OAuthClient`Private`camelCase@x], Rule["Object", x_String] :> Rule["Object", OAuthClient`Private`camelCase@x],"Metadata":>MetaInformation};
						data = Replace[data,{Rule[rule_,Null]:>Rule[rule,Missing["NotAvailable"]]},Infinity];
						Dataset@(Association[Replace[#, {r : {___Rule} :> Association[r]}, -1]] & /@ data)
						)]
     	]			
]

stripecookeddata["AddRefund",id_, args_?(MatchQ[#,{___Rule}]&)] :=  Block[{invalidParameters,params={},chargeid,amount,money,currency,refcurr,fee,reason,metadata,
												body,raw,predata,data,errmsg},

	invalidParameters = Select[Keys[args],!MemberQ[{"ChargeID","Amount","RefundApplicationFee","Reason",MetaInformation},#]&]; 
	
	If[Length[invalidParameters]>0,
		(
			Message[ServiceObject::noget,#,"Stripe"]&/@invalidParameters;
			Throw[$Failed]
		)];

	If[KeyExistsQ[args,"ChargeID"],
		(
			chargeid = "ChargeID" /. args;
			If[!StringQ[chargeid],
				(
				Message[ServiceExecute::nval,"ChargeID","Stripe"];
				Throw[$Failed]
				)
			];
		),
		Message[ServiceExecute::nparam,"ChargeID","Stripe"];
		Throw[$Failed]
	];

	If[KeyExistsQ[args,"Amount"],
		(
		money = "Amount"/.args;
			If[!QuantityQ[money],
				Message[ServiceExecute::nval,"Amount","Stripe"];
				Throw[$Failed]
			];
		currency = QuantityUnit[money]/.$StripeCurrencyToCode;
		refcurr = "currency"/.(stripeimportdata@OAuthClient`rawoauthdata[id,"RawChargeRetrieve",{"id"->chargeid}]);
		amount = QuantityMagnitude[money];
			If[!MemberQ[$StripeNoCent,currency],
			amount = 100*amount;
			];
			If[Or[!MemberQ[$StripeCurrencies,currency],!MatchQ[amount, x_ /; MemberQ[{Real, Integer}, Head[x]] && (x == Round[x]) && x>=0],!(refcurr===currency)],
			Message[ServiceExecute::nval,"Amount","Stripe"];
			Throw[$Failed]
			];
		amount = ToString@Round@amount;
		params = Append[params,Rule["amount",amount]];
		)
	];
	
	If[KeyExistsQ[args,"RefundApplicationFee"],
		(
		fee = "RefundApplicationFee"/.args;
			If[!BooleanQ[fee],
				Message[ServiceExecute::nval,"Fee","Stripe"];
				Throw[$Failed]
			];
		params = Append[params,Rule["refund_application_fee",ToLowerCase@ToString[fee]]];
		)
	];
	
	If[KeyExistsQ[args,"Reason"],
		reason = "Reason" /. args;
		If[!MemberQ[{"Duplicate", "Fraudulent", "RequestedByCustomer"},reason],
			(
			Message[ServiceExecute::nval,"Reason","Stripe"];
			Throw[$Failed]
			),
		params = Append[params,Rule["reason",reason/.{"Duplicate"->"duplicate", "Fraudulent"->"fraudulent", "RequestedByCustomer"->"requested_by_customer"}]];
		]
	];	
	
	If[KeyExistsQ[args,MetaInformation],
		(
			metadata = MetaInformation/.args;
			If[!Or[AssociationQ[metadata], Head[metadata] === List],
				Message[ServiceExecute::nval,MetaInformation,"Stripe"];
				Throw[$Failed]
			];
			params = Join[params, (Rule["metadata[" <> First[#] <> "]",Last[#]]) & /@ Normal@metadata];
		)
	];

	If[!(params === {}), body = Fold[#1 <> "&" <> #2 &, First[#] <> "=" <> Last[#] & /@ params], body = ""];

	raw = stripeimportdata@OAuthClient`rawoauthdata[id,"RawRefundCreate",{"id"->chargeid,"ParameterlessBodyData"->body}];
	If[!KeyExistsQ[#, "error"],
		(
			predata = Module[{quant, unix, dateObj},
				quant = Replace[#, List[a___, "amount" -> x_, b___, "currency" -> y_, c___] :> List[a, b, Rule["amount", If[MemberQ[$StripeNoCent, y], Quantity[x, y],NumberForm[N[#], {10, 2}, NumberPadding -> {"", "0"}, ExponentFunction -> (Null &)]&@Quantity[(x/100), y]]], c], {0}];
				unix = quant //. Rule["created", x_Integer] :> DateObject[AbsoluteTime[{1970, 1, 1}] + x, "TimeZone" -> 0];
				dateObj = unix //. List[a___, x_DateObject, b___] :> List[a, Rule["created", x], b]]
		),
		errmsg = "message" /. ("error" /. raw);
		Message[ServiceExecute::apierr,errmsg];
		Throw[$Failed]
		]&@ raw;
		
	predata = Replace[predata, x_Rule /; First[x] === "metadata" :> Rule[First[x], placeholder[First[#]] -> Last[#] & /@ Last[x]], Infinity];
	predata = Replace[predata, {Rule[string_String, x_] :> Rule[OAuthClient`Private`camelCase@string, x]}, Infinity];
	data = Replace[predata, x_Rule /; Head@First[x] === placeholder :> Rule[First[First[x]], Last[x]], Infinity];
	data = data //. {Rule["Reason", x_String] :> Rule["Reason", OAuthClient`Private`camelCase@x], Rule["Object", x_String] :> Rule["Object", OAuthClient`Private`camelCase@x],"Metadata":>MetaInformation};
	data = Replace[data,{Rule[rule_,Null]:>Rule[rule,Missing["NotAvailable"]]},Infinity];
	Dataset@Association[Replace[data, {r : {___Rule} :> Association[r]}, -1]]
]

stripecookeddata["UpdateRefund",id_, args_?(MatchQ[#,{___Rule}]&)] :=  Block[{invalidParameters,params={},chargeid,refundid,metadata,body,raw,predata,data,errmsg},

	invalidParameters = Select[Keys[args],!MemberQ[{"ChargeID","RefundID",MetaInformation},#]&]; 
	
	If[Length[invalidParameters]>0,
		(
			Message[ServiceObject::noget,#,"Stripe"]&/@invalidParameters;
			Throw[$Failed]
		)];
	
	If[KeyExistsQ[args,"ChargeID"],
		(
			chargeid = "ChargeID" /. args;
			If[!StringQ[chargeid],
				(
				Message[ServiceExecute::nval,"ChargeID","Stripe"];
				Throw[$Failed]
				)
			];
		),
		Message[ServiceExecute::nparam,"ChargeID","Stripe"];
		Throw[$Failed]
	];
	
	If[KeyExistsQ[args,"RefundID"],
		(
			refundid = "RefundID" /. args;
			If[!StringQ[refundid],
				(
				Message[ServiceExecute::nval,"RefundID","Stripe"];
				Throw[$Failed]
				)
			];
		),
		Message[ServiceExecute::nparam,"RefundID","Stripe"];
		Throw[$Failed]
	];	

	If[KeyExistsQ[args,MetaInformation],
		(
			metadata = MetaInformation/.args;
			If[!Or[AssociationQ[metadata], Head[metadata] === List],
				Message[ServiceExecute::nval,MetaInformation,"Stripe"];
				Throw[$Failed]
			];
			params = Join[params, (Rule["metadata[" <> First[#] <> "]",Last[#]]) & /@ Normal@metadata];
		)
	];

	If[!(params === {}), body = Fold[#1 <> "&" <> #2 &, First[#] <> "=" <> Last[#] & /@ params], body = ""];

	raw = stripeimportdata@OAuthClient`rawoauthdata[id,"RawRefundUpdate",{"id"->refundid,"charge"->chargeid,"ParameterlessBodyData"->body}];
	If[!KeyExistsQ[#, "error"],
		(
			predata = Module[{quant, unix, dateObj},
				quant = Replace[#, List[a___, "amount" -> x_, b___, "currency" -> y_, c___] :> List[a, b, Rule["amount", If[MemberQ[$StripeNoCent, y], Quantity[x, y],NumberForm[N[#], {10, 2}, NumberPadding -> {"", "0"}, ExponentFunction -> (Null &)]&@Quantity[(x/100), y]]], c], {0}];
				unix = quant //. Rule["created", x_Integer] :> DateObject[AbsoluteTime[{1970, 1, 1}] + x, "TimeZone" -> 0];
				dateObj = unix //. List[a___, x_DateObject, b___] :> List[a, Rule["created", x], b]]
		),
		errmsg = "message" /. ("error" /. raw);
		Message[ServiceExecute::apierr,errmsg];
		Throw[$Failed]
		]&@ raw;
		
	predata = Replace[predata, x_Rule /; First[x] === "metadata" :> Rule[First[x], placeholder[First[#]] -> Last[#] & /@ Last[x]], Infinity];
	predata = Replace[predata, {Rule[string_String, x_] :> Rule[OAuthClient`Private`camelCase@string, x]}, Infinity];
	data = Replace[predata, x_Rule /; Head@First[x] === placeholder :> Rule[First[First[x]], Last[x]], Infinity];
	data = data //. {Rule["Reason", x_String] :> Rule["Reason", OAuthClient`Private`camelCase@x], Rule["Object", x_String] :> Rule["Object", OAuthClient`Private`camelCase@x],"Metadata":>MetaInformation};
	data = Replace[data,{Rule[rule_,Null]:>Rule[rule,Missing["NotAvailable"]]},Infinity];
	Dataset@Association[Replace[data, {r : {___Rule} :> Association[r]}, -1]]
		
]

stripecookeddata["ListCustomers",id_, args_?(MatchQ[#,{___Rule}]&)] :=  Block[{invalidParameters,params={},customerid,created,limit,raw,predata,data,errmsg},

	invalidParameters = Select[Keys[args],!MemberQ[{"CustomerID","StartDate","MaxItems"},#]&]; 
	
	If[Length[invalidParameters]>0,
		(
			Message[ServiceObject::noget,#,"Stripe"]&/@invalidParameters;
			Throw[$Failed]
		)];
		
	Which[
				KeyExistsQ[args,"CustomerID"] && !Fold[Or[#1, #2] &, (KeyExistsQ[args, #] &) /@ {"StartDate","MaxItems"}],
					customerid = "CustomerID" /. args;
					If[!StringQ[customerid],
						(
						Message[ServiceExecute::nval,"CustomerID","Stripe"];
						Throw[$Failed]
					),
					params = Append[params,Rule["id",customerid]];
					];
					
					raw = stripeimportdata@OAuthClient`rawoauthdata[id,"RawCustomerRetrieve",params];
					If[!KeyExistsQ[#, "error"],
						(
							predata = Module[{raw1},
								raw1=Replace[#,List["account_balance"->x_,"created"->y_,a___,"object"->"customer",b___,"currency"->z_,c___]:>List[Rule["account_balance",NumberForm[N[#],{10,2},NumberPadding->{"","0"},ExponentFunction->(Null&)]&@Quantity[x/100,"USDollars"]],"created"->DateObject[AbsoluteTime[{1970,1,1}]+y,"TimeZone"->0],a,"object"->"Customer",b,"currency"->If[StringQ[z],ToUpperCase[z],z],c],{0}];
								raw1[[5,2]]= Module[{unixrules,unixplan,planamount,coupon,unixcoupon,values},
									unixrules=Replace[#,{Rule["current_period_end",x_Integer]:>Rule["current_period_end",DateObject[AbsoluteTime[{1970,1,1}]+x,"TimeZone"->0]],Rule["start",y_Integer]:>Rule["start",DateObject[AbsoluteTime[{1970,1,1}]+y,"TimeZone"->0]],Rule["current_period_start",z_Integer]:>Rule["current_period_start",DateObject[AbsoluteTime[{1970,1,1}]+z,"TimeZone"->0]],Rule["canceled_at",w_Integer]:>Rule["canceled_at",DateObject[AbsoluteTime[{1970,1,1}]+w,"TimeZone"->0]],Rule["ended_at",u_Integer]:>Rule["ended_at",DateObject[AbsoluteTime[{1970,1,1}]+u,"TimeZone"->0]],Rule["trial_end",s_Integer]:>Rule["trial_end",DateObject[AbsoluteTime[{1970,1,1}]+s,"TimeZone"->0]],Rule["trial_start",t_Integer]:>Rule["trial_start",DateObject[AbsoluteTime[{1970,1,1}]+t,"TimeZone"->0]]},{1}];
									unixplan=Replace[unixrules,{Rule["created",z_Integer]:>Rule["created",DateObject[AbsoluteTime[{1970,1,1}]+z,"TimeZone"->0]]},{3}];
									planamount=Replace[unixplan,{List[a___,Rule["currency",x_],b___,Rule["amount",y_],c___]:>List[a,b,Rule["amount",If[MemberQ[$StripeNoCent,x],Quantity[y,x],NumberForm[N[#],{10,2},NumberPadding->{"","0"},ExponentFunction->(Null&)]&@Quantity[(y/100),x]]],c]},{2}];
									coupon = Replace[planamount,{List[a___Rule,Rule["amount_off",x_],Rule["currency",y_],b___Rule]:>List[a,If[x===Null,Rule["amount",Missing["NotAvailable"]],Rule["amount",If[MemberQ[$StripeNoCent,y],Quantity[x,y],NumberForm[N[#],{10,2},NumberPadding->{"","0"},ExponentFunction->(Null&)]&@Quantity[(x/100),y]]]],b]},{4}];
									unixcoupon = Replace[coupon,{Rule["created",x_Integer]:>Rule["created",DateObject[AbsoluteTime[{1970,1,1}]+x,"TimeZone"->0]],Rule["redeem_by",y_Integer]:>Rule["redeem_by",DateObject[AbsoluteTime[{1970,1,1}]+y,"TimeZone"->0]],Rule["start",z_Integer]:>Rule["start",DateObject[AbsoluteTime[{1970,1,1}]+z,"TimeZone"->0]],Rule["end",w_Integer]:>Rule["end",DateObject[AbsoluteTime[{1970,1,1}]+w,"TimeZone"->0]]},{3,5}];
									values=(unixcoupon//.{Rule["interval",x_String]:>Rule["interval",OAuthClient`Private`camelCase@x],Rule["status",y_String]:>Rule["status",OAuthClient`Private`camelCase@y],Rule["duration",y_String]:>Rule["duration",OAuthClient`Private`camelCase@y]})]&/@raw1[[5,2,1,2]];
								raw1[[7,2]]=Module[{unix,amount,obj},
									unix=Replace[#,{Rule["created",x_Integer]:>Rule["created",DateObject[AbsoluteTime[{1970,1,1}]+x,"TimeZone"->0]],Rule["start",y_Integer]:>Rule["start",DateObject[AbsoluteTime[{1970,1,1}]+y,"TimeZone"->0]],Rule["end",z_Integer]:>Rule["end",DateObject[AbsoluteTime[{1970,1,1}]+z,"TimeZone"->0]],Rule["redeem_by",w_Integer]:>Rule["redeem_by",DateObject[AbsoluteTime[{1970,1,1}]+w,"TimeZone"->0]]},{1,3}];
									obj=Replace[unix,{Rule["duration",n_String]:>Rule["duration",OAuthClient`Private`camelCase@n]},{3}];
									amount = Replace[obj,{List[a___Rule,Rule["amount_off",x_],Rule["currency",y_],b___Rule]:>List[a,If[x===Null,Rule["amount",Missing["NotAvailable"]],Rule["amount",If[MemberQ[$StripeNoCent,y],Quantity[x,y],NumberForm[N[#],{10,2},NumberPadding->{"","0"},ExponentFunction->(Null&)]&@Quantity[(x/100),y]]]],b]},{2}]]&@raw1[[7,2]];
								raw1[[13,2]]=Module[{expDate,funObj,countryp},
									expDate=Replace[#,{a___,Rule["exp_month",x_],b___,Rule["exp_year",y_],c___}:>{a,Rule["ExpirationDate",DateObject[{y,x}]],b,c},{0}];
									funObj=expDate//.{Rule["funding",m_String]:>Rule["funding",OAuthClient`Private`camelCase@m],"cvc_check":>"CVCCheck"};
									countryp = funObj/.{Rule["country",x_String]:>Rule["country",Interpreter["Country"][x]]}]&/@raw1[[13,2,1,2]];
								raw1]
						),
						errmsg = "message" /. ("error" /. raw);
						Message[ServiceExecute::apierr,errmsg];
						Throw[$Failed]
						]&@ raw;
					predata = Replace[predata, x_Rule /; First[x] === "metadata" :> Rule[First[x], placeholder[First[#]] -> Last[#] & /@ Last[x]], Infinity];
					predata = Replace[predata, {Rule[string_String, x_] :> Rule[OAuthClient`Private`camelCase@string, x]}, Infinity];
					data = Replace[predata, x_Rule /; Head@First[x] === placeholder :> Rule[First[First[x]], Last[x]], Infinity];
					data = data //. {Rule["Object", p_String] :> Rule["Object", OAuthClient`Private`camelCase@p],Rule["CVCCheck", p_String] :> Rule["CVCCheck", OAuthClient`Private`camelCase@p],"Metadata":>MetaInformation};
					data = Replace[data,{Rule[rule_,Null]:>Rule[rule,Missing["NotAvailable"]]},Infinity];
					Dataset@Association[Replace[data, {r : {___Rule} :> Association[r]}, -1]]

				,
					
				KeyExistsQ[args,"CustomerID"] && Fold[Or[#1, #2] &, (KeyExistsQ[args, #] &) /@ {"StartDate","MaxItems"}],
					Message[ServiceExecute::invid,"ListCustomers","CustomerID"];
					Throw[$Failed]	
					
				,
				
				!KeyExistsQ[args,"CustomerID"] && Fold[Or[#1, #2] &, (KeyExistsQ[args, #] &) /@ {"StartDate","MaxItems"}],
					If[KeyExistsQ[args,"StartDate"],
						(
						created = "StartDate"/.args;
							If[!DateObjectQ[created],
								Message[ServiceExecute::nval,"StartDate","Stripe"];
								Throw[$Failed]
							];
						params = Append[params,Rule["created[gte]",ToString[Round@AbsoluteTime[created]-AbsoluteTime[{1970, 1, 1}]]]];
						)
					];
					
					If[KeyExistsQ[args,"MaxItems"],
						(
						limit = "MaxItems"/.args;
							If[!(IntegerQ[limit] && limit>0),
								Message[ServiceExecute::nval,"MaxItems","Stripe"];
								Throw[$Failed]
							];
						params = Append[params,Rule["limit",ToString[limit]]];
						)
					];
					
					raw = stripeimportdata@OAuthClient`rawoauthdata[id,"RawCustomerList",params];
					If[KeyExistsQ[raw, "error"],
						errmsg = "message" /. ("error" /. raw);
						Message[ServiceExecute::apierr,errmsg];
						Throw[$Failed],
						(
							predata = Module[{raw1},
								raw1=Replace[#,List["account_balance"->x_,"created"->y_,a___,"object"->"customer",b___,"currency"->z_,c___]:>List[Rule["account_balance",NumberForm[N[#],{10,2},NumberPadding->{"","0"},ExponentFunction->(Null&)]&@Quantity[x/100,"USDollars"]],"created"->DateObject[AbsoluteTime[{1970,1,1}]+y,"TimeZone"->0],a,"object"->"Customer",b,"currency"->If[StringQ[z],ToUpperCase[z],z],c],{0}];
								raw1[[5,2]]= Module[{unixrules,unixplan,planamount,coupon,unixcoupon,values},
									unixrules=Replace[#,{Rule["current_period_end",x_Integer]:>Rule["current_period_end",DateObject[AbsoluteTime[{1970,1,1}]+x,"TimeZone"->0]],Rule["start",y_Integer]:>Rule["start",DateObject[AbsoluteTime[{1970,1,1}]+y,"TimeZone"->0]],Rule["current_period_start",z_Integer]:>Rule["current_period_start",DateObject[AbsoluteTime[{1970,1,1}]+z,"TimeZone"->0]],Rule["canceled_at",w_Integer]:>Rule["canceled_at",DateObject[AbsoluteTime[{1970,1,1}]+w,"TimeZone"->0]],Rule["ended_at",u_Integer]:>Rule["ended_at",DateObject[AbsoluteTime[{1970,1,1}]+u,"TimeZone"->0]],Rule["trial_end",s_Integer]:>Rule["trial_end",DateObject[AbsoluteTime[{1970,1,1}]+s,"TimeZone"->0]],Rule["trial_start",t_Integer]:>Rule["trial_start",DateObject[AbsoluteTime[{1970,1,1}]+t,"TimeZone"->0]]},{1}];
									unixplan=Replace[unixrules,{Rule["created",z_Integer]:>Rule["created",DateObject[AbsoluteTime[{1970,1,1}]+z,"TimeZone"->0]]},{3}];
									planamount=Replace[unixplan,{List[a___,Rule["currency",x_],b___,Rule["amount",y_],c___]:>List[a,b,Rule["amount",If[MemberQ[$StripeNoCent,x],Quantity[y,x],NumberForm[N[#],{10,2},NumberPadding->{"","0"},ExponentFunction->(Null&)]&@Quantity[(y/100),x]]],c]},{2}];
									coupon = Replace[planamount,{List[a___Rule,Rule["amount_off",x_],Rule["currency",y_],b___Rule]:>List[a,If[x===Null,Rule["amount",Missing["NotAvailable"]],Rule["amount",If[MemberQ[$StripeNoCent,y],Quantity[x,y],NumberForm[N[#],{10,2},NumberPadding->{"","0"},ExponentFunction->(Null&)]&@Quantity[(x/100),y]]]],b]},{4}];
									unixcoupon = Replace[coupon,{Rule["created",x_Integer]:>Rule["created",DateObject[AbsoluteTime[{1970,1,1}]+x,"TimeZone"->0]],Rule["redeem_by",y_Integer]:>Rule["redeem_by",DateObject[AbsoluteTime[{1970,1,1}]+y,"TimeZone"->0]],Rule["start",z_Integer]:>Rule["start",DateObject[AbsoluteTime[{1970,1,1}]+z,"TimeZone"->0]],Rule["end",w_Integer]:>Rule["end",DateObject[AbsoluteTime[{1970,1,1}]+w,"TimeZone"->0]]},{3,5}];
									values=(unixcoupon//.{Rule["interval",x_String]:>Rule["interval",OAuthClient`Private`camelCase@x],Rule["status",y_String]:>Rule["status",OAuthClient`Private`camelCase@y],Rule["duration",y_String]:>Rule["duration",OAuthClient`Private`camelCase@y]})]&/@raw1[[5,2,1,2]];
								raw1[[7,2]]=Module[{unix,amount,obj},
									unix=Replace[#,{Rule["created",x_Integer]:>Rule["created",DateObject[AbsoluteTime[{1970,1,1}]+x,"TimeZone"->0]],Rule["start",y_Integer]:>Rule["start",DateObject[AbsoluteTime[{1970,1,1}]+y,"TimeZone"->0]],Rule["end",z_Integer]:>Rule["end",DateObject[AbsoluteTime[{1970,1,1}]+z,"TimeZone"->0]],Rule["redeem_by",w_Integer]:>Rule["redeem_by",DateObject[AbsoluteTime[{1970,1,1}]+w,"TimeZone"->0]]},{1,3}];
									obj=Replace[unix,{Rule["duration",n_String]:>Rule["duration",OAuthClient`Private`camelCase@n]},{3}];
									amount = Replace[obj,{List[a___Rule,Rule["amount_off",x_],Rule["currency",y_],b___Rule]:>List[a,If[x===Null,Rule["amount",Missing["NotAvailable"]],Rule["amount",If[MemberQ[$StripeNoCent,y],Quantity[x,y],NumberForm[N[#],{10,2},NumberPadding->{"","0"},ExponentFunction->(Null&)]&@Quantity[(x/100),y]]]],b]},{2}]]&@raw1[[7,2]];
								raw1[[13,2]]=Module[{expDate,funObj,countryp},
									expDate=Replace[#,{a___,Rule["exp_month",x_],b___,Rule["exp_year",y_],c___}:>{a,Rule["ExpirationDate",DateObject[{y,x}]],b,c},{0}];
									funObj=expDate//.{Rule["funding",m_String]:>Rule["funding",OAuthClient`Private`camelCase@m],"cvc_check":>"CVCCheck"};
									countryp = funObj/.{Rule["country",x_String]:>Rule["country",Interpreter["Country"][x]]}]&/@raw1[[13,2,1,2]];
								raw1]& /@ ("data" /.raw);
					
						predata = Replace[predata, x_Rule /; First[x] === "metadata" :> Rule[First[x], placeholder[First[#]] -> Last[#] & /@ Last[x]], Infinity];
						predata = Replace[predata, {Rule[string_String, x_] :> Rule[OAuthClient`Private`camelCase@string, x]}, Infinity];
						data = Replace[predata, x_Rule /; Head@First[x] === placeholder :> Rule[First[First[x]], Last[x]], Infinity];
						data = ReplaceRepeated[data ,{Rule["Object", p_String] :> Rule["Object", OAuthClient`Private`camelCase@p],Rule["CVCCheck", p_String] :> Rule["CVCCheck", OAuthClient`Private`camelCase@p],"Metadata":>MetaInformation}];
						data = Replace[data,{Rule[rule_,Null]:>Rule[rule,Missing["NotAvailable"]]},Infinity];
						Dataset[Association[Replace[#, {r : {___Rule} :> Association[r]}, -1]] & /@ data]
						)]
					
				,
				
				True,
					raw = stripeimportdata@OAuthClient`rawoauthdata[id,"RawCustomerList",{}];
					If[KeyExistsQ[raw, "error"],
						errmsg = "message" /. ("error" /. raw);
						Message[ServiceExecute::apierr,errmsg];
						Throw[$Failed],
						(
							predata = Module[{raw1},
								raw1=Replace[#,List["account_balance"->x_,"created"->y_,a___,"object"->"customer",b___,"currency"->z_,c___]:>List[Rule["account_balance",NumberForm[N[#],{10,2},NumberPadding->{"","0"},ExponentFunction->(Null&)]&@Quantity[x/100,"USDollars"]],"created"->DateObject[AbsoluteTime[{1970,1,1}]+y,"TimeZone"->0],a,"object"->"Customer",b,"currency"->If[StringQ[z],ToUpperCase[z],z],c],{0}];
								raw1[[5,2]]= Module[{unixrules,unixplan,planamount,coupon,unixcoupon,values},
									unixrules=Replace[#,{Rule["current_period_end",x_Integer]:>Rule["current_period_end",DateObject[AbsoluteTime[{1970,1,1}]+x,"TimeZone"->0]],Rule["start",y_Integer]:>Rule["start",DateObject[AbsoluteTime[{1970,1,1}]+y,"TimeZone"->0]],Rule["current_period_start",z_Integer]:>Rule["current_period_start",DateObject[AbsoluteTime[{1970,1,1}]+z,"TimeZone"->0]],Rule["canceled_at",w_Integer]:>Rule["canceled_at",DateObject[AbsoluteTime[{1970,1,1}]+w,"TimeZone"->0]],Rule["ended_at",u_Integer]:>Rule["ended_at",DateObject[AbsoluteTime[{1970,1,1}]+u,"TimeZone"->0]],Rule["trial_end",s_Integer]:>Rule["trial_end",DateObject[AbsoluteTime[{1970,1,1}]+s,"TimeZone"->0]],Rule["trial_start",t_Integer]:>Rule["trial_start",DateObject[AbsoluteTime[{1970,1,1}]+t,"TimeZone"->0]]},{1}];
									unixplan=Replace[unixrules,{Rule["created",z_Integer]:>Rule["created",DateObject[AbsoluteTime[{1970,1,1}]+z,"TimeZone"->0]]},{3}];
									planamount=Replace[unixplan,{List[a___,Rule["currency",x_],b___,Rule["amount",y_],c___]:>List[a,b,Rule["amount",If[MemberQ[$StripeNoCent,x],Quantity[y,x],NumberForm[N[#],{10,2},NumberPadding->{"","0"},ExponentFunction->(Null&)]&@Quantity[(y/100),x]]],c]},{2}];
									coupon = Replace[planamount,{List[a___Rule,Rule["amount_off",x_],Rule["currency",y_],b___Rule]:>List[a,If[x===Null,Rule["amount",Missing["NotAvailable"]],Rule["amount",If[MemberQ[$StripeNoCent,y],Quantity[x,y],NumberForm[N[#],{10,2},NumberPadding->{"","0"},ExponentFunction->(Null&)]&@Quantity[(x/100),y]]]],b]},{4}];
									unixcoupon = Replace[coupon,{Rule["created",x_Integer]:>Rule["created",DateObject[AbsoluteTime[{1970,1,1}]+x,"TimeZone"->0]],Rule["redeem_by",y_Integer]:>Rule["redeem_by",DateObject[AbsoluteTime[{1970,1,1}]+y,"TimeZone"->0]],Rule["start",z_Integer]:>Rule["start",DateObject[AbsoluteTime[{1970,1,1}]+z,"TimeZone"->0]],Rule["end",w_Integer]:>Rule["end",DateObject[AbsoluteTime[{1970,1,1}]+w,"TimeZone"->0]]},{3,5}];
									values=(unixcoupon//.{Rule["interval",x_String]:>Rule["interval",OAuthClient`Private`camelCase@x],Rule["status",y_String]:>Rule["status",OAuthClient`Private`camelCase@y],Rule["duration",y_String]:>Rule["duration",OAuthClient`Private`camelCase@y]})]&/@raw1[[5,2,1,2]];
								raw1[[7,2]]=Module[{unix,amount,obj},
									unix=Replace[#,{Rule["created",x_Integer]:>Rule["created",DateObject[AbsoluteTime[{1970,1,1}]+x,"TimeZone"->0]],Rule["start",y_Integer]:>Rule["start",DateObject[AbsoluteTime[{1970,1,1}]+y,"TimeZone"->0]],Rule["end",z_Integer]:>Rule["end",DateObject[AbsoluteTime[{1970,1,1}]+z,"TimeZone"->0]],Rule["redeem_by",w_Integer]:>Rule["redeem_by",DateObject[AbsoluteTime[{1970,1,1}]+w,"TimeZone"->0]]},{1,3}];
									obj=Replace[unix,{Rule["duration",n_String]:>Rule["duration",OAuthClient`Private`camelCase@n]},{3}];
									amount = Replace[obj,{List[a___Rule,Rule["amount_off",x_],Rule["currency",y_],b___Rule]:>List[a,If[x===Null,Rule["amount",Missing["NotAvailable"]],Rule["amount",If[MemberQ[$StripeNoCent,y],Quantity[x,y],NumberForm[N[#],{10,2},NumberPadding->{"","0"},ExponentFunction->(Null&)]&@Quantity[(x/100),y]]]],b]},{2}]]&@raw1[[7,2]];
								raw1[[13,2]]=Module[{expDate,funObj,countryp},
									expDate=Replace[#,{a___,Rule["exp_month",x_],b___,Rule["exp_year",y_],c___}:>{a,Rule["ExpirationDate",DateObject[{y,x}]],b,c},{0}];
									funObj=expDate//.{Rule["funding",m_String]:>Rule["funding",OAuthClient`Private`camelCase@m],"cvc_check":>"CVCCheck"};
									countryp = funObj/.{Rule["country",x_String]:>Rule["country",Interpreter["Country"][x]]}]&/@raw1[[13,2,1,2]];
								raw1]& /@ ("data" /.raw);
					
						predata = Replace[predata, x_Rule /; First[x] === "metadata" :> Rule[First[x], placeholder[First[#]] -> Last[#] & /@ Last[x]], Infinity];
						predata = Replace[predata, {Rule[string_String, x_] :> Rule[OAuthClient`Private`camelCase@string, x]}, Infinity];
						data = Replace[predata, x_Rule /; Head@First[x] === placeholder :> Rule[First[First[x]], Last[x]], Infinity];
						data = ReplaceRepeated[data ,{Rule["Object", p_String] :> Rule["Object", OAuthClient`Private`camelCase@p],Rule["CVCCheck", p_String] :> Rule["CVCCheck", OAuthClient`Private`camelCase@p],"Metadata":>MetaInformation}];
						data = Replace[data,{Rule[rule_,Null]:>Rule[rule,Missing["NotAvailable"]]},Infinity];
						Dataset[Association[Replace[#, {r : {___Rule} :> Association[r]}, -1]] & /@ data]
						)]
     	]			
]

stripecookeddata["AddCustomer",id_, args_?(MatchQ[#,{___Rule}]&)] :=  Block[{invalidParameters,params={},cparams={},balance,currency,coupon,description,email,plan,planid,planquant,
												payment,number,expdate,expyear,expmon,cvc,cardtoken,trialend,tax,metadata,body,raw,predata,data,errmsg},

	invalidParameters = Select[Keys[args],!MemberQ[{"Balance","CouponCode","Description","Email",MetaInformation,"Plan","TaxFraction","PaymentMethod","TrialEndDate"},#]&];
	
	If[Length[invalidParameters]>0,
		(
			Message[ServiceObject::noget,#,"Stripe"]&/@invalidParameters;
			Throw[$Failed]
		)];
					
		If[KeyExistsQ[args,"Balance"],
			(
			balance = "Balance"/.args;
			If[!QuantityQ[balance],
				Message[ServiceExecute::nval,"Balance","Stripe"];
				Throw[$Failed]
			];
			currency = QuantityUnit[balance]/.$StripeCurrencyToCode;
			If[!MemberQ[$StripeCurrencies,currency],
				(
				Message[ServiceExecute::nval,"Balance","Stripe"];
				Throw[$Failed]
				)
			];
			balance = Round@(100 QuantityMagnitude@UnitConvert[balance, "USDollars"]);
			params = Append[params,Rule["account_balance",ToString@balance]];
			)
		];
	
	If[KeyExistsQ[args,"CouponCode"],
		coupon = "CouponCode"/.args;
		If[!StringQ[coupon],
			(
			Message[ServiceExecute::nval,"CouponCode","Stripe"];
			Throw[$Failed]
			),
		params = Append[params,Rule["coupon",coupon]]
		];
	];
	
	If[KeyExistsQ[args,"Description"],
		description = "Description"/.args;
		If[!StringQ[description],
			(
			Message[ServiceExecute::nval,"Description","Stripe"];
			Throw[$Failed]
			),
		params = Append[params,Rule["description",description]]
		];
	];
		
	If[KeyExistsQ[args,"Email"],
		email = "Email"/.args;
		If[!StringQ[email],
			(
			Message[ServiceExecute::nval,"Email","Stripe"];
			Throw[$Failed]
			),
		params = Append[params,Rule["email",email]]
		];
	];

	If[KeyExistsQ[args,MetaInformation],
		(
			metadata = MetaInformation/.args;
			If[!Or[AssociationQ[metadata], Head[metadata] === List],
				Message[ServiceExecute::nval,MetaInformation,"Stripe"];
				Throw[$Failed]
			];
			params = Join[params, (Rule["metadata[" <> First[#] <> "]",Last[#]]) & /@ Normal@metadata];
		)
	];

	If[KeyExistsQ[args,"Plan"],
		plan = "Plan"/.args;
		If[!AssociationQ[plan],
			(
			Message[ServiceExecute::nval,"Plan","Stripe"];
			Throw[$Failed]
			),
			
			If[KeyExistsQ[plan,"ID"],
				(
				planid = "ID" /. plan;
				If[!StringQ[planid],
					(
					Message[ServiceExecute::nval,"ID","Stripe"];
					Throw[$Failed]
					),
				params = Append[params,Rule["plan",planid]]];
				),
				Message[ServiceExecute::nparam,"ID","Stripe"];
				Throw[$Failed]
				];
				
			If[KeyExistsQ[plan,"Quantity"],
				planquant = "Quantity"/.plan;
				If[!(IntegerQ[planquant] && planquant > 0),
					(
					Message[ServiceExecute::nval,"PlanParameter","Stripe"];
					Throw[$Failed]
					),
				params = Append[params,Rule["quantity",ToString@planquant]]
				];
			];			
		];
	];
	
	(**NOT REQUESTED BUT IMPLEMENTED DUE TO ITS UTILITY**)
	If[KeyExistsQ[args,"TaxFraction"],
		tax = "TaxFraction"/.args;
		If[!(MatchQ[#,x_ /; MemberQ[{Real, Integer, Rational}, Head[x]] && 100 > x >= 1 && (100 x == Round[100 x])]&@tax),
			(
			Message[ServiceExecute::nval,"TaxFraction","Stripe"];
			Throw[$Failed]
			),
		params = Append[params,Rule["tax_percent",ToString@N[#, {4, 3}] &@tax]]
		];
	];
	(*******************************************************************)
	
	If[KeyExistsQ[args,"PaymentMethod"],
					
		payment = "PaymentMethod"/.args;
		
		If[!AssociationQ[payment],
			Message[ServiceExecute::nval,"PaymentMethod","Stripe"];
			Throw[$Failed]
			];
		
		invalidParameters = Select[Keys[payment],!MemberQ[{"Number","ExpirationDate","CVC"},#]&];
	
		If[Length[invalidParameters]>0,
			(
				Message[ServiceObject::noget,#,"Stripe"]&/@invalidParameters;
				Throw[$Failed]
			)];			
					
		If[KeyExistsQ[payment,"Number"],
			(
			number = "Number"/.payment;
				If[!Or[StringQ[number],IntegerQ[number]],
					Message[ServiceExecute::nval,"Number","Stripe"];
					Throw[$Failed]
				];
			number = ToString@number;
			cparams = Append[cparams,Rule["card[number]",number]];
			),
			Message[ServiceExecute::mparam,"Number","Stripe"];
			Throw[$Failed]
			];
					
		If[KeyExistsQ[payment,"ExpirationDate"],
			(
			expdate = "ExpirationDate"/.payment;
				If[!DateObjectQ[expdate],
					Message[ServiceExecute::nval,"ExpirationDate","Stripe"];
					Throw[$Failed]
				];
			expyear = ToString@DateList[expdate][[1]];
			expmon = ToString@DateList[expdate][[2]];
			cparams = Append[cparams,Rule["card[exp_year]",expyear]];
			cparams = Append[cparams,Rule["card[exp_month]",expmon]];
			),
			Message[ServiceExecute::mparam,"ExpirationDate","Stripe"];
			Throw[$Failed]
		];
	
			If[KeyExistsQ[payment,"CVC"],
				(
				cvc = "CVC"/.payment;
					If[!Or[StringQ[cvc],IntegerQ[cvc]],
						Message[ServiceExecute::nval,"CVC","Stripe"];
						Throw[$Failed]
					];
				cvc = ToString@cvc;
				cparams = Append[cparams,Rule["card[cvc]",cvc]];
				)
		];
	cardtoken = stripeimportdata@OAuthClient`rawoauthdata[id,"RawGetCardKey",cparams];
	If[KeyExistsQ[cardtoken, "error"],
		errmsg = "message" /. ("error" /. cardtoken);
		Message[ServiceExecute::apierr,errmsg];
		Throw[$Failed]
		];
	params = Append[params,Rule["source","id"/.cardtoken]];
	];

	If[KeyExistsQ[args,"TrialEndDate"],
		(
		trialend = "TrialEndDate"/.args;
			If[!DateObjectQ[trialend],
				Message[ServiceExecute::nval,"TrialEndDate","Stripe"];
				Throw[$Failed]
			];
		params = Append[params,Rule["trial_end",ToString[Round@AbsoluteTime[trialend]-AbsoluteTime[{1970, 1, 1}]]]];
		)
	];

	If[!(params === {}), body = Fold[#1 <> "&" <> #2 &, First[#] <> "=" <> Last[#] & /@ params], body = ""];
	raw = stripeimportdata@OAuthClient`rawoauthdata[id,"RawCustomerCreate",{"ParameterlessBodyData"->body}];
	If[!KeyExistsQ[#, "error"],
		(
			predata = Module[{raw1},
				raw1=Replace[#,List["account_balance"->x_,"created"->y_,a___,"object"->"customer",b___,"currency"->z_,c___]:>List[Rule["account_balance",NumberForm[N[#],{10,2},NumberPadding->{"","0"},ExponentFunction->(Null&)]&@Quantity[x/100,"USDollars"]],"created"->DateObject[AbsoluteTime[{1970,1,1}]+y,"TimeZone"->0],a,"object"->"Customer",b,"currency"->If[StringQ[z],ToUpperCase[z],z],c],{0}];
				raw1[[5,2]]= Module[{unixrules,unixplan,planamount,couponp,unixcoupon,values},
					unixrules=Replace[#,{Rule["current_period_end",x_Integer]:>Rule["current_period_end",DateObject[AbsoluteTime[{1970,1,1}]+x,"TimeZone"->0]],Rule["start",y_Integer]:>Rule["start",DateObject[AbsoluteTime[{1970,1,1}]+y,"TimeZone"->0]],Rule["current_period_start",z_Integer]:>Rule["current_period_start",DateObject[AbsoluteTime[{1970,1,1}]+z,"TimeZone"->0]],Rule["canceled_at",w_Integer]:>Rule["canceled_at",DateObject[AbsoluteTime[{1970,1,1}]+w,"TimeZone"->0]],Rule["ended_at",u_Integer]:>Rule["ended_at",DateObject[AbsoluteTime[{1970,1,1}]+u,"TimeZone"->0]],Rule["trial_end",s_Integer]:>Rule["trial_end",DateObject[AbsoluteTime[{1970,1,1}]+s,"TimeZone"->0]],Rule["trial_start",t_Integer]:>Rule["trial_start",DateObject[AbsoluteTime[{1970,1,1}]+t,"TimeZone"->0]]},{1}];
					unixplan=Replace[unixrules,{Rule["created",z_Integer]:>Rule["created",DateObject[AbsoluteTime[{1970,1,1}]+z,"TimeZone"->0]]},{3}];
					planamount=Replace[unixplan,{List[a___,Rule["currency",x_],b___,Rule["amount",y_],c___]:>List[a,b,Rule["amount",If[MemberQ[$StripeNoCent,x],Quantity[y,x],NumberForm[N[#],{10,2},NumberPadding->{"","0"},ExponentFunction->(Null&)]&@Quantity[(y/100),x]]],c]},{2}];
					couponp = Replace[planamount,{List[a___Rule,Rule["amount_off",x_],Rule["currency",y_],b___Rule]:>List[a,If[x===Null,Rule["amount",Missing["NotAvailable"]],Rule["amount",If[MemberQ[$StripeNoCent,y],Quantity[x,y],NumberForm[N[#],{10,2},NumberPadding->{"","0"},ExponentFunction->(Null&)]&@Quantity[(x/100),y]]]],b]},{4}];
					unixcoupon = Replace[couponp,{Rule["created",x_Integer]:>Rule["created",DateObject[AbsoluteTime[{1970,1,1}]+x,"TimeZone"->0]],Rule["redeem_by",y_Integer]:>Rule["redeem_by",DateObject[AbsoluteTime[{1970,1,1}]+y,"TimeZone"->0]],Rule["start",z_Integer]:>Rule["start",DateObject[AbsoluteTime[{1970,1,1}]+z,"TimeZone"->0]],Rule["end",w_Integer]:>Rule["end",DateObject[AbsoluteTime[{1970,1,1}]+w,"TimeZone"->0]]},{3,5}];
					values=(unixcoupon//.{Rule["interval",x_String]:>Rule["interval",OAuthClient`Private`camelCase@x],Rule["status",y_String]:>Rule["status",OAuthClient`Private`camelCase@y],Rule["duration",y_String]:>Rule["duration",OAuthClient`Private`camelCase@y]})]&/@raw1[[5,2,1,2]];
				raw1[[7,2]]=Module[{unix,amount,obj},
					unix=Replace[#,{Rule["created",x_Integer]:>Rule["created",DateObject[AbsoluteTime[{1970,1,1}]+x,"TimeZone"->0]],Rule["start",y_Integer]:>Rule["start",DateObject[AbsoluteTime[{1970,1,1}]+y,"TimeZone"->0]],Rule["end",z_Integer]:>Rule["end",DateObject[AbsoluteTime[{1970,1,1}]+z,"TimeZone"->0]],Rule["redeem_by",w_Integer]:>Rule["redeem_by",DateObject[AbsoluteTime[{1970,1,1}]+w,"TimeZone"->0]]},{1,3}];
					obj=Replace[unix,{Rule["duration",n_String]:>Rule["duration",OAuthClient`Private`camelCase@n]},{3}];
					amount = Replace[obj,{List[a___Rule,Rule["amount_off",x_],Rule["currency",y_],b___Rule]:>List[a,If[x===Null,Rule["amount",Missing["NotAvailable"]],Rule["amount",If[MemberQ[$StripeNoCent,y],Quantity[x,y],NumberForm[N[#],{10,2},NumberPadding->{"","0"},ExponentFunction->(Null&)]&@Quantity[(x/100),y]]]],b]},{2}]]&@raw1[[7,2]];
				raw1[[13,2]]=Module[{expDate,funObj,countryp},
					expDate=Replace[#,{a___,Rule["exp_month",x_],b___,Rule["exp_year",y_],c___}:>{a,Rule["ExpirationDate",DateObject[{y,x}]],b,c},{0}];
					funObj=expDate//.{Rule["funding",m_String]:>Rule["funding",OAuthClient`Private`camelCase@m],"cvc_check":>"CVCCheck"};
					countryp = funObj/.{Rule["country",x_String]:>Rule["country",Interpreter["Country"][x]]}]&/@raw1[[13,2,1,2]];
				raw1]
		),
		errmsg = "message" /. ("error" /. raw);
		Message[ServiceExecute::apierr,errmsg];
		Throw[$Failed]
	]&@ raw;
		
		predata = Replace[predata, x_Rule /; First[x] === "metadata" :> Rule[First[x], placeholder[First[#]] -> Last[#] & /@ Last[x]], Infinity];
		predata = Replace[predata, {Rule[string_String, x_] :> Rule[OAuthClient`Private`camelCase@string, x]}, Infinity];
		data = Replace[predata, x_Rule /; Head@First[x] === placeholder :> Rule[First[First[x]], Last[x]], Infinity];
		data = data //. {Rule["Object", p_String] :> Rule["Object", OAuthClient`Private`camelCase@p],Rule["CVCCheck", p_String] :> Rule["CVCCheck", OAuthClient`Private`camelCase@p],"Metadata":>MetaInformation};
		data = Replace[data,{Rule[rule_,Null]:>Rule[rule,Missing["NotAvailable"]]},Infinity];
		Dataset@Association[Replace[data, {r : {___Rule} :> Association[r]}, -1]]

]

stripecookeddata["UpdateCustomer",id_, args_?(MatchQ[#,{___Rule}]&)] :=  Block[{invalidParameters,params={},cparams={},customer,balance,currency,coupon,description,email,default,
												payment,number,expdate,expyear,expmon,cvc,cardtoken,metadata,body,raw,predata,data,errmsg},

	invalidParameters = Select[Keys[args],!MemberQ[{"CustomerID","Balance","DefaultPaymentMethod","CouponCode","Description","Email",MetaInformation,"PaymentMethod"},#]&]; 
	
	If[Length[invalidParameters]>0,
		(
			Message[ServiceObject::noget,#,"Stripe"]&/@invalidParameters;
			Throw[$Failed]
		)];
		
	If[KeyExistsQ[args,"CustomerID"],
		(
			customer = "CustomerID" /. args;
			If[!StringQ[customer],
				(
				Message[ServiceExecute::nval,"CustomerID","Stripe"];
				Throw[$Failed]
				)
			];
		),
		Message[ServiceExecute::nparam,"CustomerID","Stripe"];
		Throw[$Failed]
	];
	
	If[KeyExistsQ[args,"Balance"],
			(
		balance = "Balance"/.args;
		If[!QuantityQ[balance],
			Message[ServiceExecute::nval,"Balance","Stripe"];
			Throw[$Failed]
		];
		currency = QuantityUnit[balance]/.$StripeCurrencyToCode;
		If[!MemberQ[$StripeCurrencies,currency],
			(
			Message[ServiceExecute::nval,"Balance","Stripe"];
			Throw[$Failed]
			)
		];
		balance = Round@(100 QuantityMagnitude@UnitConvert[balance, "USDollars"]);
		params = Append[params,Rule["account_balance",ToString@balance]];
		)
	];
	
	If[KeyExistsQ[args,"CouponCode"],
		coupon = "CouponCode"/.args;
		If[!StringQ[coupon],
			(
			Message[ServiceExecute::nval,"CouponCode","Stripe"];
			Throw[$Failed]
			),
		params = Append[params,Rule["coupon",coupon]]
		];
	];
	
	If[KeyExistsQ[args,"Description"],
		description = "Description"/.args;
		If[!StringQ[description],
			(
			Message[ServiceExecute::nval,"Description","Stripe"];
			Throw[$Failed]
			),
		params = Append[params,Rule["description",description]]
		];
	];
	
	If[KeyExistsQ[args,"DefaultPaymentMethod"],
		default = "DefaultPaymentMethod"/.args;
		If[!StringQ[default],
			(
			Message[ServiceExecute::nval,"DefaultPaymentMethod","Stripe"];
			Throw[$Failed]
			),
		params = Append[params,Rule["default_source",default]]
		];
	];	
		
	If[KeyExistsQ[args,"Email"],
		email = "Email"/.args;
		If[!StringQ[email],
			(
			Message[ServiceExecute::nval,"Email","Stripe"];
			Throw[$Failed]
			),
		params = Append[params,Rule["email",email]]
		];
	];

	If[KeyExistsQ[args,MetaInformation],
		(
			metadata = MetaInformation/.args;
			If[!Or[AssociationQ[metadata], Head[metadata] === List],
				Message[ServiceExecute::nval,MetaInformation,"Stripe"];
				Throw[$Failed]
			];
			params = Join[params, (Rule["metadata[" <> First[#] <> "]",Last[#]]) & /@ Normal@metadata];
		)
	];

	If[KeyExistsQ[args,"PaymentMethod"],
					
		payment = "PaymentMethod"/.args;
		
		If[!AssociationQ[payment],
			Message[ServiceExecute::nval,"PaymentMethod","Stripe"];
			Throw[$Failed]
			];
		
		invalidParameters = Select[Keys[payment],!MemberQ[{"Number","ExpirationDate","CVC"},#]&];
	
		If[Length[invalidParameters]>0,
			(
				Message[ServiceObject::noget,#,"Stripe"]&/@invalidParameters;
				Throw[$Failed]
			)];			
					
		If[KeyExistsQ[payment,"Number"],
			(
			number = "Number"/.payment;
				If[!Or[StringQ[number],IntegerQ[number]],
					Message[ServiceExecute::nval,"Number","Stripe"];
					Throw[$Failed]
				];
			number = ToString@number;
			cparams = Append[cparams,Rule["card[number]",number]];
			),
			Message[ServiceExecute::mparam,"Number","Stripe"];
			Throw[$Failed]
			];
					
		If[KeyExistsQ[payment,"ExpirationDate"],
			(
			expdate = "ExpirationDate"/.payment;
				If[!DateObjectQ[expdate],
					Message[ServiceExecute::nval,"ExpirationDate","Stripe"];
					Throw[$Failed]
				];
			expyear = ToString@DateList[expdate][[1]];
			expmon = ToString@DateList[expdate][[2]];
			cparams = Append[cparams,Rule["card[exp_year]",expyear]];
			cparams = Append[cparams,Rule["card[exp_month]",expmon]];
			),
			Message[ServiceExecute::mparam,"ExpirationDate","Stripe"];
			Throw[$Failed]
		];
	
		If[KeyExistsQ[payment,"CVC"],
			(
			cvc = "CVC"/.payment;
				If[!Or[StringQ[cvc],IntegerQ[cvc]],
					Message[ServiceExecute::nval,"CVC","Stripe"];
					Throw[$Failed]
				];
			cvc = ToString@cvc;
			cparams = Append[cparams,Rule["card[cvc]",cvc]];
			)
		];
		cardtoken = stripeimportdata@OAuthClient`rawoauthdata[id,"RawGetCardKey",cparams];
		If[KeyExistsQ[cardtoken, "error"],
			errmsg = "message" /. ("error" /. cardtoken);
			Message[ServiceExecute::apierr,errmsg];
			Throw[$Failed]
			];
		params = Append[params,Rule["source","id"/.cardtoken]];
	];

	If[!(params === {}), body = Fold[#1 <> "&" <> #2 &, First[#] <> "=" <> Last[#] & /@ params], body = ""];
	
	raw = stripeimportdata@OAuthClient`rawoauthdata[id,"RawCustomerUpdate",{"id"->customer,"ParameterlessBodyData"->body}];
	If[!KeyExistsQ[#, "error"],
		(
			predata = Module[{raw1},
				raw1=Replace[#,List["account_balance"->x_,"created"->y_,a___,"object"->"customer",b___,"currency"->z_,c___]:>List[Rule["account_balance",NumberForm[N[#],{10,2},NumberPadding->{"","0"},ExponentFunction->(Null&)]&@Quantity[x/100,"USDollars"]],"created"->DateObject[AbsoluteTime[{1970,1,1}]+y,"TimeZone"->0],a,"object"->"Customer",b,"currency"->If[StringQ[z],ToUpperCase[z],z],c],{0}];
				raw1[[5,2]]= Module[{unixrules,unixplan,planamount,couponp,unixcoupon,values},
					unixrules=Replace[#,{Rule["current_period_end",x_Integer]:>Rule["current_period_end",DateObject[AbsoluteTime[{1970,1,1}]+x,"TimeZone"->0]],Rule["start",y_Integer]:>Rule["start",DateObject[AbsoluteTime[{1970,1,1}]+y,"TimeZone"->0]],Rule["current_period_start",z_Integer]:>Rule["current_period_start",DateObject[AbsoluteTime[{1970,1,1}]+z,"TimeZone"->0]],Rule["canceled_at",w_Integer]:>Rule["canceled_at",DateObject[AbsoluteTime[{1970,1,1}]+w,"TimeZone"->0]],Rule["ended_at",u_Integer]:>Rule["ended_at",DateObject[AbsoluteTime[{1970,1,1}]+u,"TimeZone"->0]],Rule["trial_end",s_Integer]:>Rule["trial_end",DateObject[AbsoluteTime[{1970,1,1}]+s,"TimeZone"->0]],Rule["trial_start",t_Integer]:>Rule["trial_start",DateObject[AbsoluteTime[{1970,1,1}]+t,"TimeZone"->0]]},{1}];
					unixplan=Replace[unixrules,{Rule["created",z_Integer]:>Rule["created",DateObject[AbsoluteTime[{1970,1,1}]+z,"TimeZone"->0]]},{3}];
					planamount=Replace[unixplan,{List[a___,Rule["currency",x_],b___,Rule["amount",y_],c___]:>List[a,b,Rule["amount",If[MemberQ[$StripeNoCent,x],Quantity[y,x],NumberForm[N[#],{10,2},NumberPadding->{"","0"},ExponentFunction->(Null&)]&@Quantity[(y/100),x]]],c]},{2}];
					couponp = Replace[planamount,{List[a___Rule,Rule["amount_off",x_],Rule["currency",y_],b___Rule]:>List[a,If[x===Null,Rule["amount",Missing["NotAvailable"]],Rule["amount",If[MemberQ[$StripeNoCent,y],Quantity[x,y],NumberForm[N[#],{10,2},NumberPadding->{"","0"},ExponentFunction->(Null&)]&@Quantity[(x/100),y]]]],b]},{4}];
					unixcoupon = Replace[couponp,{Rule["created",x_Integer]:>Rule["created",DateObject[AbsoluteTime[{1970,1,1}]+x,"TimeZone"->0]],Rule["redeem_by",y_Integer]:>Rule["redeem_by",DateObject[AbsoluteTime[{1970,1,1}]+y,"TimeZone"->0]],Rule["start",z_Integer]:>Rule["start",DateObject[AbsoluteTime[{1970,1,1}]+z,"TimeZone"->0]],Rule["end",w_Integer]:>Rule["end",DateObject[AbsoluteTime[{1970,1,1}]+w,"TimeZone"->0]]},{3,5}];
					values=(unixcoupon//.{Rule["interval",x_String]:>Rule["interval",OAuthClient`Private`camelCase@x],Rule["status",y_String]:>Rule["status",OAuthClient`Private`camelCase@y],Rule["duration",y_String]:>Rule["duration",OAuthClient`Private`camelCase@y]})]&/@raw1[[5,2,1,2]];
				raw1[[7,2]]=Module[{unix,amount,obj},
					unix=Replace[#,{Rule["created",x_Integer]:>Rule["created",DateObject[AbsoluteTime[{1970,1,1}]+x,"TimeZone"->0]],Rule["start",y_Integer]:>Rule["start",DateObject[AbsoluteTime[{1970,1,1}]+y,"TimeZone"->0]],Rule["end",z_Integer]:>Rule["end",DateObject[AbsoluteTime[{1970,1,1}]+z,"TimeZone"->0]],Rule["redeem_by",w_Integer]:>Rule["redeem_by",DateObject[AbsoluteTime[{1970,1,1}]+w,"TimeZone"->0]]},{1,3}];
					obj=Replace[unix,{Rule["duration",n_String]:>Rule["duration",OAuthClient`Private`camelCase@n]},{3}];
					amount = Replace[obj,{List[a___Rule,Rule["amount_off",x_],Rule["currency",y_],b___Rule]:>List[a,If[x===Null,Rule["amount",Missing["NotAvailable"]],Rule["amount",If[MemberQ[$StripeNoCent,y],Quantity[x,y],NumberForm[N[#],{10,2},NumberPadding->{"","0"},ExponentFunction->(Null&)]&@Quantity[(x/100),y]]]],b]},{2}]]&@raw1[[7,2]];
				raw1[[13,2]]=Module[{expDate,funObj,countryp},
					expDate=Replace[#,{a___,Rule["exp_month",x_],b___,Rule["exp_year",y_],c___}:>{a,Rule["ExpirationDate",DateObject[{y,x}]],b,c},{0}];
					funObj=expDate//.{Rule["funding",m_String]:>Rule["funding",OAuthClient`Private`camelCase@m],"cvc_check":>"CVCCheck"};
					countryp = funObj/.{Rule["country",x_String]:>Rule["country",Interpreter["Country"][x]]}]&/@raw1[[13,2,1,2]];
				raw1]
		),
		errmsg = "message" /. ("error" /. raw);
		Message[ServiceExecute::apierr,errmsg];
		Throw[$Failed]
	]&@ raw;
		
	predata = Replace[predata, x_Rule /; First[x] === "metadata" :> Rule[First[x], placeholder[First[#]] -> Last[#] & /@ Last[x]], Infinity];
	predata = Replace[predata, {Rule[string_String, x_] :> Rule[OAuthClient`Private`camelCase@string, x]}, Infinity];
	data = Replace[predata, x_Rule /; Head@First[x] === placeholder :> Rule[First[First[x]], Last[x]], Infinity];
	data = data //. {Rule["Object", p_String] :> Rule["Object", OAuthClient`Private`camelCase@p],Rule["CVCCheck", p_String] :> Rule["CVCCheck", OAuthClient`Private`camelCase@p],"Metadata":>MetaInformation};
	data = Replace[data,{Rule[rule_,Null]:>Rule[rule,Missing["NotAvailable"]]},Infinity];
	Dataset@Association[Replace[data, {r : {___Rule} :> Association[r]}, -1]]
	
]

stripecookeddata["RemoveCustomer",id_, args_?(MatchQ[#,{___Rule}]&)] :=  Block[{invalidParameters,params={},customer,raw,deleted,data,errmsg},

	invalidParameters = Select[Keys[args],!MemberQ[{"CustomerID"},#]&]; 
	
	If[Length[invalidParameters]>0,
		(
			Message[ServiceObject::noget,#,"Stripe"]&/@invalidParameters;
			Throw[$Failed]
		)];
		
	If[KeyExistsQ[args,"CustomerID"],
		(
			customer = "CustomerID" /. args;
			If[!StringQ[customer],
				(
				Message[ServiceExecute::nval,"CustomerID","Stripe"];
				Throw[$Failed]
				),
			raw = stripeimportdata@OAuthClient`rawoauthdata[id,"RawCustomerDelete",{"id"->customer}];
			If[KeyExistsQ[raw, "error"],
				(
				errmsg = "message" /. ("error" /. raw);
				Message[ServiceExecute::apierr,errmsg];
				Throw[$Failed]
				)];
			deleted = "deleted"/.raw;
			data = "id" /. raw;
			If[deleted,data]
			]
		),
		Message[ServiceExecute::nparam,"CustomerID","Stripe"];
		Throw[$Failed]
	]
]

stripecookeddata["ListPaymentMethods",id_, args_?(MatchQ[#,{___Rule}]&)] :=  Block[{invalidParameters,params={},customer,cardid,limit,raw,predata,data,errmsg},

	invalidParameters = Select[Keys[args],!MemberQ[{"CustomerID","PaymentMethodID","MaxItems"},#]&]; 
	
	If[Length[invalidParameters]>0,
		(
			Message[ServiceObject::noget,#,"Stripe"]&/@invalidParameters;
			Throw[$Failed]
		)];
		
	If[KeyExistsQ[args,"CustomerID"],
		(
		customer = "CustomerID" /. args;
		If[!StringQ[customer],
			(
			Message[ServiceExecute::nval,"CustomerID","Stripe"];
			Throw[$Failed]
			)];
		),
		Message[ServiceExecute::nparam,"CustomerID","Stripe"];
		Throw[$Failed]
	];
		
	Which[
				KeyExistsQ[args,"PaymentMethodID"] && !KeyExistsQ[args,"MaxItems"],
					cardid = "PaymentMethodID" /. args;
					If[!StringQ[cardid],
						(
						Message[ServiceExecute::nval,"PaymentMethodID","Stripe"];
						Throw[$Failed]
					),
					params = Append[params,Rule["id",cardid]];
					params = Append[params,Rule["customer",customer]]
					];
					
					raw = stripeimportdata@OAuthClient`rawoauthdata[id,"RawCardRetrieve",params];
					If[!KeyExistsQ[#, "error"],
						(
							predata = Module[{quant,countryp,cvc},
								quant = Replace[#, List[a___, "exp_month" -> x_, b___, "exp_year" -> y_, c___] :> List[a,Rule["ExpirationDate",DateObject[{y, x}]], b, c], {0}];
								countryp = quant/.{Rule["country",x_String] :> Rule["country",Interpreter["Country"][x]]};
								cvc = countryp/.{"cvc_check":>"CVCCheck"}]
						),
						errmsg = "message" /. ("error" /. raw);
						Message[ServiceExecute::apierr,errmsg];
						Throw[$Failed]
						]&@ raw;
					predata = Replace[predata, x_Rule /; First[x] === "metadata" :> Rule[First[x], placeholder[First[#]] -> Last[#] & /@ Last[x]], Infinity];
					predata = Replace[predata, {Rule[string_String, x_] :> Rule[OAuthClient`Private`camelCase@string, x]}, Infinity];
					data = Replace[predata, x_Rule /; Head@First[x] === placeholder :> Rule[First[First[x]], Last[x]], Infinity];
					data = data //. {Rule["Funding", x_String] :> Rule["Funding", OAuthClient`Private`camelCase@x],Rule["Object", y_String] :> Rule["Object", OAuthClient`Private`camelCase@y],Rule["CVCCheck", z_String] :> Rule["CVCCheck", OAuthClient`Private`camelCase@z],"Metadata":>MetaInformation};
					data = Replace[data,{Rule[rule_,Null]:>Rule[rule,Missing["NotAvailable"]]},Infinity];
					Dataset@Association[Replace[data, {r : {___Rule} :> Association[r]}, -1]]

				,
					
				KeyExistsQ[args,"PaymentMethodID"] && KeyExistsQ[args,"MaxItems"],
					Message[ServiceExecute::invid,"ListPaymentMethods","CustomerID and PaymentMethodID"];
					Throw[$Failed]	
					
				,
				
				!KeyExistsQ[args,"PaymentMethodID"] && KeyExistsQ[args,"MaxItems"],
					params = Append[params,Rule["id",customer]];
					limit = "MaxItems" /. args;
					If[!(IntegerQ[limit] && limit>0),
						(
							Message[ServiceExecute::nval,"MaxItems","Stripe"];
							Throw[$Failed]
						),
						params = Append[params,Rule["limit",ToString[limit]]];
						];
					
					raw = stripeimportdata@OAuthClient`rawoauthdata[id,"RawCardList",params];
					If[KeyExistsQ[raw, "error"],
						errmsg = "message" /. ("error" /. raw);
						Message[ServiceExecute::apierr,errmsg];
						Throw[$Failed],
						(
							predata = Module[{quant,countryp,cvc},
								quant = Replace[#, List[a___, "exp_month" -> x_, b___, "exp_year" -> y_, c___] :> List[a,Rule["ExpirationDate",DateObject[{y, x}]], b, c], {0}];
								countryp = quant/.{Rule["country",x_String] :> Rule["country",Interpreter["Country"][x]]};
								cvc = countryp/.{"cvc_check":>"CVCCheck"}
							]& /@ ("data" /.raw);
					
						predata = Replace[predata, x_Rule /; First[x] === "metadata" :> Rule[First[x], placeholder[First[#]] -> Last[#] & /@ Last[x]], Infinity];
						predata = Replace[predata, {Rule[string_String, x_] :> Rule[OAuthClient`Private`camelCase@string, x]}, Infinity];
						data = Replace[predata, x_Rule /; Head@First[x] === placeholder :> Rule[First[First[x]], Last[x]], Infinity];
						data = data //. {Rule["Funding", x_String] :> Rule["Funding", OAuthClient`Private`camelCase@x],Rule["Object", y_String] :> Rule["Object", OAuthClient`Private`camelCase@y],Rule["CVCCheck", z_String] :> Rule["CVCCheck", OAuthClient`Private`camelCase@z],"Metadata":>MetaInformation};
						data = Replace[data,{Rule[rule_,Null]:>Rule[rule,Missing["NotAvailable"]]},Infinity];
						Dataset[Association[Replace[#, {r : {___Rule} :> Association[r]}, -1]] & /@ data]
						)]
					
				,
				
				True,
					raw = stripeimportdata@OAuthClient`rawoauthdata[id,"RawCardList",{"id"->customer}];
					If[KeyExistsQ[raw, "error"],
						errmsg = "message" /. ("error" /. raw);
						Message[ServiceExecute::apierr,errmsg];
						Throw[$Failed],
						(
							predata = Module[{quant,countryp,cvc},
								quant = Replace[#, List[a___, "exp_month" -> x_, b___, "exp_year" -> y_, c___] :> List[a,Rule["ExpirationDate",DateObject[{y, x}]], b, c], {0}];
								countryp = quant/.{Rule["country",x_String] :> Rule["country",Interpreter["Country"][x]]};
								cvc = countryp/.{"cvc_check":>"CVCCheck"}
							]& /@ ("data" /.raw);
					
						predata = Replace[predata, x_Rule /; First[x] === "metadata" :> Rule[First[x], placeholder[First[#]] -> Last[#] & /@ Last[x]], Infinity];
						predata = Replace[predata, {Rule[string_String, x_] :> Rule[OAuthClient`Private`camelCase@string, x]}, Infinity];
						data = Replace[predata, x_Rule /; Head@First[x] === placeholder :> Rule[First[First[x]], Last[x]], Infinity];
						data = data //. {Rule["Funding", x_String] :> Rule["Funding", OAuthClient`Private`camelCase@x],Rule["Object", y_String] :> Rule["Object", OAuthClient`Private`camelCase@y],Rule["CVCCheck", z_String] :> Rule["CVCCheck", OAuthClient`Private`camelCase@z],"Metadata":>MetaInformation};
						data = Replace[data,{Rule[rule_,Null]:>Rule[rule,Missing["NotAvailable"]]},Infinity];
						Dataset[Association[Replace[#, {r : {___Rule} :> Association[r]}, -1]] & /@ data]
						)]
		]		
]

stripecookeddata["AddPaymentMethod",id_, args_?(MatchQ[#,{___Rule}]&)] :=  Block[{invalidParameters,params={},cparams={},customer,payment,number,expdate,expyear,expmon,cvc,cardtoken,raw,predata,data,errmsg},

	invalidParameters = Select[Keys[args],!MemberQ[{"CustomerID","PaymentMethod"},#]&];
	
	If[Length[invalidParameters]>0,
		(
			Message[ServiceObject::noget,#,"Stripe"]&/@invalidParameters;
			Throw[$Failed]
		)];

	If[KeyExistsQ[args,"CustomerID"],
		(
		customer = "CustomerID" /. args;
		If[!StringQ[customer],
			(
			Message[ServiceExecute::nval,"CustomerID","Stripe"];
			Throw[$Failed]
			)];
		params = Append[params,Rule["id",customer]]
		),
		Message[ServiceExecute::nparam,"CustomerID","Stripe"];
		Throw[$Failed]
	];
	
	If[KeyExistsQ[args,"PaymentMethod"],
					
		payment = "PaymentMethod"/.args;
		
		If[!AssociationQ[payment],
			Message[ServiceExecute::nval,"PaymentMethod","Stripe"];
			Throw[$Failed]
			];

		invalidParameters = Select[Keys[payment],!MemberQ[{"Number","ExpirationDate","CVC"},#]&];
	
		If[Length[invalidParameters]>0,
			(
				Message[ServiceObject::noget,#,"Stripe"]&/@invalidParameters;
				Throw[$Failed]
			)];	
					
		If[KeyExistsQ[payment,"Number"],
			(
			number = "Number"/.payment;
				If[!Or[StringQ[number],IntegerQ[number]],
					Message[ServiceExecute::nval,"Number","Stripe"];
					Throw[$Failed]
				];
			number = ToString@number;
			cparams = Append[cparams,Rule["card[number]",number]];
			),
			Message[ServiceExecute::mparam,"Number","Stripe"];
			Throw[$Failed]
			];
					
		If[KeyExistsQ[payment,"ExpirationDate"],
			(
			expdate = "ExpirationDate"/.payment;
				If[!DateObjectQ[expdate],
					Message[ServiceExecute::nval,"ExpirationDate","Stripe"];
					Throw[$Failed]
				];
			expyear = ToString@DateList[expdate][[1]];
			expmon = ToString@DateList[expdate][[2]];
			cparams = Append[cparams,Rule["card[exp_year]",expyear]];
			cparams = Append[cparams,Rule["card[exp_month]",expmon]];
			),
			Message[ServiceExecute::mparam,"ExpirationDate","Stripe"];
			Throw[$Failed]
		];
	
			If[KeyExistsQ[payment,"CVC"],
				(
				cvc = "CVC"/.payment;
					If[!Or[StringQ[cvc],IntegerQ[cvc]],
						Message[ServiceExecute::nval,"CVC","Stripe"];
						Throw[$Failed]
					];
				cvc = ToString@cvc;
				cparams = Append[cparams,Rule["card[cvc]",cvc]];
				)
		];
		cardtoken = stripeimportdata@OAuthClient`rawoauthdata[id,"RawGetCardKey",cparams];
		If[KeyExistsQ[cardtoken, "error"],
			errmsg = "message" /. ("error" /. cardtoken);
			Message[ServiceExecute::apierr,errmsg];
			Throw[$Failed]
			];
		params = Append[params,Rule["source","id"/.cardtoken]];
	];

	raw = stripeimportdata@OAuthClient`rawoauthdata[id,"RawCardCreate",params];
	
	If[!KeyExistsQ[#, "error"],
		(
			predata = Module[{quant,countryp,cvcp},
				quant = Replace[#, List[a___, "exp_month" -> x_, b___, "exp_year" -> y_, c___] :> List[a,Rule["ExpirationDate",DateObject[{y, x}]], b, c], {0}];
				countryp = quant/.{Rule["country",x_String] :> Rule["country",Interpreter["Country"][x]]};
				cvcp = countryp/.{"cvc_check":>"CVCCheck"}]
		),
		errmsg = "message" /. ("error" /. raw);
		Message[ServiceExecute::apierr,errmsg];
		Throw[$Failed]
		]&@ raw;
	predata = Replace[predata, x_Rule /; First[x] === "metadata" :> Rule[First[x], placeholder[First[#]] -> Last[#] & /@ Last[x]], Infinity];
	predata = Replace[predata, {Rule[string_String, x_] :> Rule[OAuthClient`Private`camelCase@string, x]}, Infinity];
	data = Replace[predata, x_Rule /; Head@First[x] === placeholder :> Rule[First[First[x]], Last[x]], Infinity];
	data = data //. {Rule["Funding", x_String] :> Rule["Funding", OAuthClient`Private`camelCase@x],Rule["Object", y_String] :> Rule["Object", OAuthClient`Private`camelCase@y],Rule["CVCCheck", z_String] :> Rule["CVCCheck", OAuthClient`Private`camelCase@z],"Metadata":>MetaInformation};
	data = Replace[data,{Rule[rule_,Null]:>Rule[rule,Missing["NotAvailable"]]},Infinity];
	Dataset@Association[Replace[data, {r : {___Rule} :> Association[r]}, -1]]

]

stripecookeddata["RemovePaymentMethod",id_, args_?(MatchQ[#,{___Rule}]&)] :=  Block[{invalidParameters,params={},customer,cardid,raw,deleted,data,errmsg},

	invalidParameters = Select[Keys[args],!MemberQ[{"CustomerID","PaymentMethodID"},#]&]; 
	
	If[Length[invalidParameters]>0,
		(
			Message[ServiceObject::noget,#,"Stripe"]&/@invalidParameters;
			Throw[$Failed]
		)];
		
	If[KeyExistsQ[args,"CustomerID"],
		(
			customer = "CustomerID" /. args;
			If[!StringQ[customer],
				(
				Message[ServiceExecute::nval,"CustomerID","Stripe"];
				Throw[$Failed]
				),
			params = Append[params,Rule["customer",customer]]
			];
		),
		Message[ServiceExecute::nparam,"CustomerID","Stripe"];
		Throw[$Failed]
	];	
		
	If[KeyExistsQ[args,"PaymentMethodID"],
		(
			cardid = "PaymentMethodID" /. args;
			If[!StringQ[cardid],
				(
				Message[ServiceExecute::nval,"PaymentMethodID","Stripe"];
				Throw[$Failed]
				),
			params = Append[params,Rule["id",cardid]];
			raw = stripeimportdata@OAuthClient`rawoauthdata[id,"RawCardDelete",params];
			If[KeyExistsQ[raw, "error"],
				(
				errmsg = "message" /. ("error" /. raw);
				Message[ServiceExecute::apierr,errmsg];
				Throw[$Failed]
				)];
			deleted = "deleted"/.raw;
			data = "id" /. raw;
			If[deleted,data]
			]
		),
		Message[ServiceExecute::nparam,"PaymentMethodID","Stripe"];
		Throw[$Failed]
	]
]

stripecookeddata["UpdatePaymentMethod",id_, args_?(MatchQ[#,{___Rule}]&)] :=  Block[{invalidParameters,params={},customer,cardid,name,address,city,country,state,line1,line2,zip,
													expdate,expmon,expyear,metadata,body,raw,predata,data,errmsg},

	invalidParameters = Select[Keys[args],!MemberQ[{"CustomerID","PaymentMethodID","Name","Address","ExpirationDate",MetaInformation},#]&]; 
	
	If[Length[invalidParameters]>0,
		(
			Message[ServiceObject::noget,#,"Stripe"]&/@invalidParameters;
			Throw[$Failed]
		)];
		
	If[KeyExistsQ[args,"CustomerID"],
		(
			customer = "CustomerID" /. args;
			If[!StringQ[customer],
				(
				Message[ServiceExecute::nval,"CustomerID","Stripe"];
				Throw[$Failed]
				)
			];
		),
		Message[ServiceExecute::nparam,"CustomerID","Stripe"];
		Throw[$Failed]
	];

	If[KeyExistsQ[args,"PaymentMethodID"],
		(
			cardid = "PaymentMethodID" /. args;
			If[!StringQ[cardid],
				(
				Message[ServiceExecute::nval,"PaymentMethodID","Stripe"];
				Throw[$Failed]
				)
			];
		),
		Message[ServiceExecute::nparam,"PaymentMethodID","Stripe"];
		Throw[$Failed]
	];
		
	If[KeyExistsQ[args,"Name"],
		name = "Name"/.args;
		If[!StringQ[name],
			(
			Message[ServiceExecute::nval,"Name","Stripe"];
			Throw[$Failed]
			),
		params = Append[params,Rule["name",name]]
		];
	];

	If[KeyExistsQ[args,"Address"],
		(
			address = "Address"/.args;
			
			If[!AssociationQ[address],
				Message[ServiceExecute::nval,"Address","Stripe"];
				Throw[$Failed]
			];
			
			invalidParameters = Select[Keys[address],!MemberQ[{"City","Country","State","FirstLine","SecondLine","ZipCode"},#]&];
	
			If[Length[invalidParameters]>0,
				(
					Message[ServiceObject::noget,#,"Stripe"]&/@invalidParameters;
					Throw[$Failed]
				)];

			If[KeyExistsQ[address,"City"],
				city = "City"/.address;			
				If[!StringQ[city],
					(
					Message[ServiceExecute::nval,"City","Stripe"];
					Throw[$Failed]
					),
				params = Append[params,Rule["address_city",city]]
				];
			];
			
			If[KeyExistsQ[address,"Country"],
				country = "Country"/.address;			
				If[!StringQ[country],
					(
					Message[ServiceExecute::nval,"Country","Stripe"];
					Throw[$Failed]
					),
				params = Append[params,Rule["address_country",country]]
				];
			];

			If[KeyExistsQ[address,"State"],
				state = "State"/.address;			
				If[!StringQ[state],
					(
					Message[ServiceExecute::nval,"State","Stripe"];
					Throw[$Failed]
					),
				params = Append[params,Rule["address_state",state]]
				];
			];

			If[KeyExistsQ[address,"FirstLine"],
				line1 = "FirstLine"/.address;			
				If[!StringQ[line1],
					(
					Message[ServiceExecute::nval,"FirstLine","Stripe"];
					Throw[$Failed]
					),
				params = Append[params,Rule["address_line1",line1]]
				];
			];

			If[KeyExistsQ[address,"SecondLine"],
				line2 = "SecondLine"/.address;			
				If[!StringQ[line2],
					(
					Message[ServiceExecute::nval,"SecondLine","Stripe"];
					Throw[$Failed]
					),
				params = Append[params,Rule["address_line2",line2]]
				];
			];

			If[KeyExistsQ[address,"ZipCode"],
				zip = "ZipCode"/.address;			
				If[!StringQ[zip],
					(
					Message[ServiceExecute::nval,"ZipCode","Stripe"];
					Throw[$Failed]
					),
				params = Append[params,Rule["address_zip",zip]]
				];
			];
		)
	];

	If[KeyExistsQ[args,"ExpirationDate"],
		expdate = "ExpirationDate"/.args;
			If[!DateObjectQ[expdate],
				Message[ServiceExecute::nval,"ExpirationDate","Stripe"];
				Throw[$Failed]
			];
		expyear = ToString@DateList[expdate][[1]];
		expmon = ToString@DateList[expdate][[2]];
		params = Append[params,Rule["exp_year",expyear]];
		params = Append[params,Rule["exp_month",expmon]];
	];

	If[KeyExistsQ[args,MetaInformation],
		(
			metadata = MetaInformation/.args;
			If[!Or[AssociationQ[metadata], Head[metadata] === List],
				Message[ServiceExecute::nval,MetaInformation,"Stripe"];
				Throw[$Failed]
			];
			params = Join[params, (Rule["metadata[" <> First[#] <> "]",Last[#]]) & /@ Normal@metadata];
		)
	];

	If[!(params === {}), body = Fold[#1 <> "&" <> #2 &, First[#] <> "=" <> Last[#] & /@ params], body = ""];
	
	raw = stripeimportdata@OAuthClient`rawoauthdata[id,"RawCardUpdate",{"customer"->customer,"id"->cardid,"ParameterlessBodyData"->body}];	
	If[!KeyExistsQ[#, "error"],
		(
			predata = Module[{quant,countryp,cvc},
				quant = Replace[#, List[a___, "exp_month" -> x_, b___, "exp_year" -> y_, c___] :> List[a,Rule["ExpirationDate",DateObject[{y, x}]], b, c], {0}];
				countryp = quant/.{Rule["country",x_String] :> Rule["country",Interpreter["Country"][x]]};
				cvc = countryp/.{"cvc_check":>"CVCCheck"}]
		),
		errmsg = "message" /. ("error" /. raw);
		Message[ServiceExecute::apierr,errmsg];
		Throw[$Failed]
		]&@ raw;
	predata = Replace[predata, x_Rule /; First[x] === "metadata" :> Rule[First[x], placeholder[First[#]] -> Last[#] & /@ Last[x]], Infinity];
	predata = Replace[predata, {Rule[string_String, x_] :> Rule[OAuthClient`Private`camelCase@string, x]}, Infinity];
	data = Replace[predata, x_Rule /; Head@First[x] === placeholder :> Rule[First[First[x]], Last[x]], Infinity];
	data = data //. {Rule["Funding", x_String] :> Rule["Funding", OAuthClient`Private`camelCase@x],Rule["Object", y_String] :> Rule["Object", OAuthClient`Private`camelCase@y],Rule["CVCCheck", z_String] :> Rule["CVCCheck", OAuthClient`Private`camelCase@z],"Metadata":>MetaInformation};
	data = Replace[data,{Rule[rule_,Null]:>Rule[rule,Missing["NotAvailable"]]},Infinity];
	Dataset@Association[Replace[data, {r : {___Rule} :> Association[r]}, -1]]

		
]

stripecookeddata["ListSubscriptions",id_, args_?(MatchQ[#,{___Rule}]&)] :=  Block[{invalidParameters,params={},customer,subscription,limit,raw,predata,data,errmsg},

	invalidParameters = Select[Keys[args],!MemberQ[{"CustomerID","SubscriptionID","MaxItems"},#]&]; 
	
	If[Length[invalidParameters]>0,
		(
			Message[ServiceObject::noget,#,"Stripe"]&/@invalidParameters;
			Throw[$Failed]
		)];
		
	If[KeyExistsQ[args,"CustomerID"],
		(
		customer = "CustomerID" /. args;
		If[!StringQ[customer],
			(
			Message[ServiceExecute::nval,"CustomerID","Stripe"];
			Throw[$Failed]
			)];
		),
		Message[ServiceExecute::nparam,"CustomerID","Stripe"];
		Throw[$Failed]
	];
		
	Which[
				KeyExistsQ[args,"SubscriptionID"] && !KeyExistsQ[args,"MaxItems"],
					subscription = "SubscriptionID" /. args;
					If[!StringQ[subscription],
						(
						Message[ServiceExecute::nval,"SubscriptionID","Stripe"];
						Throw[$Failed]
					),
					params = Append[params,Rule["id",subscription]];
					params = Append[params,Rule["customer",customer]]
					];
					
					raw = stripeimportdata@OAuthClient`rawoauthdata[id,"RawSubscriptionRetrieve",params];
					If[!KeyExistsQ[#, "error"],
						(
							predata = Module[{unixrules, unixdiscount, unixplan, planamount},
								unixrules = Replace[#, {Rule["current_period_end", x_Integer] :> Rule["current_period_end", DateObject[AbsoluteTime[{1970, 1, 1}] + x, "TimeZone" -> 0]], Rule["start", y_Integer] :> Rule["start",DateObject[AbsoluteTime[{1970, 1, 1}] + y,"TimeZone" -> 0]], Rule["current_period_start", z_Integer] :> Rule["current_period_start", DateObject[AbsoluteTime[{1970, 1, 1}] + z, "TimeZone" -> 0]], Rule["canceled_at", w_Integer] :> Rule["canceled_at", DateObject[AbsoluteTime[{1970, 1, 1}] + w,"TimeZone" -> 0]],Rule["ended_at", u_Integer] :> Rule["ended_at", DateObject[AbsoluteTime[{1970, 1, 1}] + u, "TimeZone" -> 0]], Rule["trial_end", s_Integer] :> Rule["trial_end", DateObject[AbsoluteTime[{1970, 1, 1}] + s, "TimeZone" -> 0]], Rule["trial_start", t_Integer] :> Rule["trial_start", DateObject[AbsoluteTime[{1970, 1, 1}] + t, "TimeZone" -> 0]]}, {1}];
								unixdiscount = Replace[unixrules,{Rule["created", x_Integer] :> Rule["created", DateObject[AbsoluteTime[{1970, 1, 1}] + x, "TimeZone" -> 0]],Rule["start", y_Integer] :> Rule["start", DateObject[AbsoluteTime[{1970, 1, 1}] + y, "TimeZone" -> 0]],Rule["end", z_Integer] :> Rule["end", DateObject[AbsoluteTime[{1970, 1, 1}] + z, "TimeZone" -> 0]],Rule["redeem_by", w_Integer] :> Rule["redeem_by", DateObject[AbsoluteTime[{1970, 1, 1}] + w, "TimeZone" -> 0]],Rule["duration",n_String]:>Rule["duration",OAuthClient`Private`camelCase@n]},{3,5}];
								unixplan = Replace[unixdiscount,{List[a___Rule,Rule["amount_off",x_],Rule["currency",y_],b___Rule]:>List[a,If[x===Null,Rule["amount",Missing["NotAvailable"]],Rule["amount",If[MemberQ[$StripeNoCent,y],Quantity[x,y],NumberForm[N[#],{10,2},NumberPadding->{"","0"},ExponentFunction->(Null&)]&@Quantity[(x/100),y]]]],b]},{4}];
								planamount = Replace[unixplan, {List[a___, Rule["currency", x_], b___, Rule["amount", y_], c___] :> List[a, b, Rule["amount", If[MemberQ[$StripeNoCent, x], Quantity[y, x], NumberForm[N[#], {10, 2}, NumberPadding -> {"", "0"}, ExponentFunction -> (Null &)] &@Quantity[(y/100), x]]], c]}, {2}]]
						),
						errmsg = "message" /. ("error" /. raw);
						Message[ServiceExecute::apierr,errmsg];
						Throw[$Failed]
						]&@ raw;
					predata = Replace[predata, x_Rule /; First[x] === "metadata" :> Rule[First[x], placeholder[First[#]] -> Last[#] & /@ Last[x]], Infinity];
					predata = Replace[predata, {Rule[string_String, x_] :> Rule[OAuthClient`Private`camelCase@string, x]}, Infinity];
					data = Replace[predata, x_Rule /; Head@First[x] === placeholder :> Rule[First[First[x]], Last[x]], Infinity];
					data = data //. {Rule["Object", x_String] :> Rule["Object", OAuthClient`Private`camelCase@x], Rule["Status", y_String] :> Rule["Status", OAuthClient`Private`camelCase@y],Rule["Interval", z_String] :> Rule["Interval", OAuthClient`Private`camelCase@z],"Metadata":>MetaInformation};
					data = Replace[data,{Rule[rule_,Null]:>Rule[rule,Missing["NotAvailable"]]},Infinity];
					Dataset@Association[Replace[data, {r : {___Rule} :> Association[r]}, -1]]

				,
					
				KeyExistsQ[args,"SubscriptionID"] && KeyExistsQ[args,"MaxItems"],
					Message[ServiceExecute::invid,"ListSubscriptions","CustomerID and SubscriptionID"];
					Throw[$Failed]	
					
				,
				
				!KeyExistsQ[args,"SubscriptionID"] && KeyExistsQ[args,"MaxItems"],
					params = Append[params,Rule["id",customer]];
					limit = "MaxItems" /. args;
					If[!(IntegerQ[limit] && limit>0),
						(
							Message[ServiceExecute::nval,"MaxItems","Stripe"];
							Throw[$Failed]
						),
						params = Append[params,Rule["limit",ToString[limit]]];
						];
					
					raw = stripeimportdata@OAuthClient`rawoauthdata[id,"RawSubscriptionList",params];
					If[KeyExistsQ[raw, "error"],
						errmsg = "message" /. ("error" /. raw);
						Message[ServiceExecute::apierr,errmsg];
						Throw[$Failed],
						(
							predata = Module[{unixrules, unixdiscount, unixplan, planamount},
								unixrules = Replace[#, {Rule["current_period_end", x_Integer] :> Rule["current_period_end", DateObject[AbsoluteTime[{1970, 1, 1}] + x, "TimeZone" -> 0]], Rule["start", y_Integer] :> Rule["start",DateObject[AbsoluteTime[{1970, 1, 1}] + y,"TimeZone" -> 0]], Rule["current_period_start", z_Integer] :> Rule["current_period_start", DateObject[AbsoluteTime[{1970, 1, 1}] + z, "TimeZone" -> 0]], Rule["canceled_at", w_Integer] :> Rule["canceled_at", DateObject[AbsoluteTime[{1970, 1, 1}] + w,"TimeZone" -> 0]],Rule["ended_at", u_Integer] :> Rule["ended_at", DateObject[AbsoluteTime[{1970, 1, 1}] + u, "TimeZone" -> 0]], Rule["trial_end", s_Integer] :> Rule["trial_end", DateObject[AbsoluteTime[{1970, 1, 1}] + s, "TimeZone" -> 0]], Rule["trial_start", t_Integer] :> Rule["trial_start", DateObject[AbsoluteTime[{1970, 1, 1}] + t, "TimeZone" -> 0]]}, {1}];
								unixdiscount = Replace[unixrules,{Rule["created", x_Integer] :> Rule["created", DateObject[AbsoluteTime[{1970, 1, 1}] + x, "TimeZone" -> 0]],Rule["start", y_Integer] :> Rule["start", DateObject[AbsoluteTime[{1970, 1, 1}] + y, "TimeZone" -> 0]],Rule["end", z_Integer] :> Rule["end", DateObject[AbsoluteTime[{1970, 1, 1}] + z, "TimeZone" -> 0]],Rule["redeem_by", w_Integer] :> Rule["redeem_by", DateObject[AbsoluteTime[{1970, 1, 1}] + w, "TimeZone" -> 0]],Rule["duration",n_String]:>Rule["duration",OAuthClient`Private`camelCase@n]},{3,5}];
								unixplan = Replace[unixdiscount,{List[a___Rule,Rule["amount_off",x_],Rule["currency",y_],b___Rule]:>List[a,If[x===Null,Rule["amount",Missing["NotAvailable"]],Rule["amount",If[MemberQ[$StripeNoCent,y],Quantity[x,y],NumberForm[N[#],{10,2},NumberPadding->{"","0"},ExponentFunction->(Null&)]&@Quantity[(x/100),y]]]],b]},{4}];
								planamount = Replace[unixplan, {List[a___, Rule["currency", x_], b___, Rule["amount", y_], c___] :> List[a, b, Rule["amount", If[MemberQ[$StripeNoCent, x], Quantity[y, x], NumberForm[N[#], {10, 2}, NumberPadding -> {"", "0"}, ExponentFunction -> (Null &)] &@Quantity[(y/100), x]]], c]}, {2}]
								]& /@ ("data" /.raw);
					
						predata = Replace[predata, x_Rule /; First[x] === "metadata" :> Rule[First[x], placeholder[First[#]] -> Last[#] & /@ Last[x]], Infinity];
						predata = Replace[predata, {Rule[string_String, x_] :> Rule[OAuthClient`Private`camelCase@string, x]}, Infinity];
						data = Replace[predata, x_Rule /; Head@First[x] === placeholder :> Rule[First[First[x]], Last[x]], Infinity];
						data = data //. {Rule["Object", x_String] :> Rule["Object", OAuthClient`Private`camelCase@x], Rule["Status", y_String] :> Rule["Status", OAuthClient`Private`camelCase@y],Rule["Interval", z_String] :> Rule["Interval", OAuthClient`Private`camelCase@z],"Metadata":>MetaInformation};
						data = Replace[data,{Rule[rule_,Null]:>Rule[rule,Missing["NotAvailable"]]},Infinity];
						Dataset[Association[Replace[#, {r : {___Rule} :> Association[r]}, -1]] & /@ data]
						)]
					
				,
				
				True,
					raw = stripeimportdata@OAuthClient`rawoauthdata[id,"RawSubscriptionList",{"id"->customer}];
					If[KeyExistsQ[raw, "error"],
						errmsg = "message" /. ("error" /. raw);
						Message[ServiceExecute::apierr,errmsg];
						Throw[$Failed],
						(
							predata = Module[{unixrules, unixdiscount, unixplan, planamount},
								unixrules = Replace[#, {Rule["current_period_end", x_Integer] :> Rule["current_period_end", DateObject[AbsoluteTime[{1970, 1, 1}] + x, "TimeZone" -> 0]], Rule["start", y_Integer] :> Rule["start",DateObject[AbsoluteTime[{1970, 1, 1}] + y,"TimeZone" -> 0]], Rule["current_period_start", z_Integer] :> Rule["current_period_start", DateObject[AbsoluteTime[{1970, 1, 1}] + z, "TimeZone" -> 0]], Rule["canceled_at", w_Integer] :> Rule["canceled_at", DateObject[AbsoluteTime[{1970, 1, 1}] + w,"TimeZone" -> 0]],Rule["ended_at", u_Integer] :> Rule["ended_at", DateObject[AbsoluteTime[{1970, 1, 1}] + u, "TimeZone" -> 0]], Rule["trial_end", s_Integer] :> Rule["trial_end", DateObject[AbsoluteTime[{1970, 1, 1}] + s, "TimeZone" -> 0]], Rule["trial_start", t_Integer] :> Rule["trial_start", DateObject[AbsoluteTime[{1970, 1, 1}] + t, "TimeZone" -> 0]]}, {1}];
								unixdiscount = Replace[unixrules,{Rule["created", x_Integer] :> Rule["created", DateObject[AbsoluteTime[{1970, 1, 1}] + x, "TimeZone" -> 0]],Rule["start", y_Integer] :> Rule["start", DateObject[AbsoluteTime[{1970, 1, 1}] + y, "TimeZone" -> 0]],Rule["end", z_Integer] :> Rule["end", DateObject[AbsoluteTime[{1970, 1, 1}] + z, "TimeZone" -> 0]],Rule["redeem_by", w_Integer] :> Rule["redeem_by", DateObject[AbsoluteTime[{1970, 1, 1}] + w, "TimeZone" -> 0]],Rule["duration",n_String]:>Rule["duration",OAuthClient`Private`camelCase@n]},{3,5}];
								unixplan = Replace[unixdiscount,{List[a___Rule,Rule["amount_off",x_],Rule["currency",y_],b___Rule]:>List[a,If[x===Null,Rule["amount",Missing["NotAvailable"]],Rule["amount",If[MemberQ[$StripeNoCent,y],Quantity[x,y],NumberForm[N[#],{10,2},NumberPadding->{"","0"},ExponentFunction->(Null&)]&@Quantity[(x/100),y]]]],b]},{4}];
								planamount = Replace[unixplan, {List[a___, Rule["currency", x_], b___, Rule["amount", y_], c___] :> List[a, b, Rule["amount", If[MemberQ[$StripeNoCent, x], Quantity[y, x], NumberForm[N[#], {10, 2}, NumberPadding -> {"", "0"}, ExponentFunction -> (Null &)] &@Quantity[(y/100), x]]], c]}, {2}]
								]& /@ ("data" /.raw);
									
						predata = Replace[predata, x_Rule /; First[x] === "metadata" :> Rule[First[x], placeholder[First[#]] -> Last[#] & /@ Last[x]], Infinity];
						predata = Replace[predata, {Rule[string_String, x_] :> Rule[OAuthClient`Private`camelCase@string, x]}, Infinity];
						data = Replace[predata, x_Rule /; Head@First[x] === placeholder :> Rule[First[First[x]], Last[x]], Infinity];						
						data = data //. {Rule["Object", x_String] :> Rule["Object", OAuthClient`Private`camelCase@x], Rule["Status", y_String] :> Rule["Status", OAuthClient`Private`camelCase@y],Rule["Interval", z_String] :> Rule["Interval", OAuthClient`Private`camelCase@z],"Metadata":>MetaInformation};
						data = Replace[data,{Rule[rule_,Null]:>Rule[rule,Missing["NotAvailable"]]},Infinity];
						Dataset[Association[Replace[#, {r : {___Rule} :> Association[r]}, -1]] & /@ data]
						)]
     	]			
]

stripecookeddata["AddSubscription",id_, args_?(MatchQ[#,{___Rule}]&)] :=  Block[{invalidParameters,params={},cparams={},customer,plan,coupon,trialend,metadata,payment,number,expdate,expmon,expyear,cvc,cardtoken,
												quant,fee,tax,body,raw,predata,data,errmsg},

	invalidParameters = Select[Keys[args],!MemberQ[{"CustomerID","PlanID","CouponCode","TrialEndDate",MetaInformation,"PaymentMethod","Quantity","ApplicationFeeFraction","TaxFraction"},#]&]; 
	
	If[Length[invalidParameters]>0,
		(
			Message[ServiceObject::noget,#,"Stripe"]&/@invalidParameters;
			Throw[$Failed]
		)];

	If[KeyExistsQ[args,"CustomerID"],
		(
			customer = "CustomerID" /. args;
			If[!StringQ[customer],
				(
				Message[ServiceExecute::nval,"CustomerID","Stripe"];
				Throw[$Failed]
				)
			];
		),
		Message[ServiceExecute::nparam,"CustomerID","Stripe"];
		Throw[$Failed]
	];

	If[KeyExistsQ[args,"PlanID"],
		(
			plan = "PlanID" /. args;
			If[!StringQ[plan],
				(
				Message[ServiceExecute::nval,"PlanID","Stripe"];
				Throw[$Failed]
				),
			params = Append[params,Rule["plan",plan]]
			];
		),
		Message[ServiceExecute::nparam,"PlanID","Stripe"];
		Throw[$Failed]
	];
	
	If[KeyExistsQ[args,"CouponCode"],
		coupon = "CouponCode"/.args;
		If[!StringQ[coupon],
			(
			Message[ServiceExecute::nval,"CouponCode","Stripe"];
			Throw[$Failed]
			),
		params = Append[params,Rule["coupon",coupon]]
		];
	];	

	If[KeyExistsQ[args,"TrialEndDate"],
		(
		trialend = "TrialEndDate"/.args;
			If[!DateObjectQ[trialend],
				Message[ServiceExecute::nval,"TrialEndDate","Stripe"];
				Throw[$Failed]
			];
		params = Append[params,Rule["trial_end",ToString[Round@AbsoluteTime[trialend]-AbsoluteTime[{1970, 1, 1}]]]];
		)
	];
	
	If[KeyExistsQ[args,MetaInformation],
		(
			metadata = MetaInformation/.args;
			If[!Or[AssociationQ[metadata], Head[metadata] === List],
				Message[ServiceExecute::nval,MetaInformation,"Stripe"];
				Throw[$Failed]
			];
			params = Join[params, (Rule["metadata[" <> First[#] <> "]",Last[#]]) & /@ Normal@metadata];
		)
	];
	
	If[KeyExistsQ[args,"PaymentMethod"],
					
		payment = "PaymentMethod"/.args;
		
		If[!AssociationQ[payment],
			Message[ServiceExecute::nval,"PaymentMethod","Stripe"];
			Throw[$Failed]
			];
		
		invalidParameters = Select[Keys[payment],!MemberQ[{"Number","ExpirationDate","CVC"},#]&];
	
		If[Length[invalidParameters]>0,
			(
				Message[ServiceObject::noget,#,"Stripe"]&/@invalidParameters;
				Throw[$Failed]
			)];			
					
		If[KeyExistsQ[payment,"Number"],
			(
			number = "Number"/.payment;
				If[!Or[StringQ[number],IntegerQ[number]],
					Message[ServiceExecute::nval,"Number","Stripe"];
					Throw[$Failed]
				];
			number = ToString@number;
			cparams = Append[cparams,Rule["card[number]",number]];
			),
			Message[ServiceExecute::mparam,"Number","Stripe"];
			Throw[$Failed]
			];
					
		If[KeyExistsQ[payment,"ExpirationDate"],
			(
			expdate = "ExpirationDate"/.payment;
				If[!DateObjectQ[expdate],
					Message[ServiceExecute::nval,"ExpirationDate","Stripe"];
					Throw[$Failed]
				];
			expyear = ToString@DateList[expdate][[1]];
			expmon = ToString@DateList[expdate][[2]];
			cparams = Append[cparams,Rule["card[exp_year]",expyear]];
			cparams = Append[cparams,Rule["card[exp_month]",expmon]];
			),
			Message[ServiceExecute::mparam,"ExpirationDate","Stripe"];
			Throw[$Failed]
		];
	
			If[KeyExistsQ[payment,"CVC"],
				(
				cvc = "CVC"/.payment;
					If[!Or[StringQ[cvc],IntegerQ[cvc]],
						Message[ServiceExecute::nval,"CVC","Stripe"];
						Throw[$Failed]
					];
				cvc = ToString@cvc;
				cparams = Append[cparams,Rule["card[cvc]",cvc]];
				)
		];
		cardtoken = stripeimportdata@OAuthClient`rawoauthdata[id,"RawGetCardKey",cparams];
		If[KeyExistsQ[cardtoken, "error"],
			errmsg = "message" /. ("error" /. cardtoken);
			Message[ServiceExecute::apierr,errmsg];
			Throw[$Failed]
			];
		params = Append[params,Rule["source","id"/.cardtoken]];
	];
	
	If[KeyExistsQ[args,"Quantity"],
		(
		quant = "Quantity"/.args;
			If[!(IntegerQ[quant] && quant>0),
				Message[ServiceExecute::nval,"Quantity","Stripe"];
				Throw[$Failed]
			];
		params = Append[params,Rule["quantity",ToString@quant]];
		)
	];	

	If[KeyExistsQ[args,"ApplicationFeeFraction"],
		fee = "ApplicationFeeFraction"/.args;
		If[!(MatchQ[#,x_ /; MemberQ[{Real, Integer, Rational}, Head[x]] && 100 > x >= 1 && (100 x == Round[100 x])]&@fee),
			(
			Message[ServiceExecute::nval,"ApplicationFeeFraction","Stripe"];
			Throw[$Failed]
			),
		params = Append[params,Rule["application_fee_percent",ToString@N[#, {4, 3}] &@fee]]
		];
	];

	If[KeyExistsQ[args,"TaxFraction"],
		tax = "TaxFraction"/.args;
		If[!(MatchQ[#,x_ /; MemberQ[{Real, Integer, Rational}, Head[x]] && 100 > x >= 1 && (100 x == Round[100 x])]&@tax),
			(
			Message[ServiceExecute::nval,"TaxFraction","Stripe"];
			Throw[$Failed]
			),
		params = Append[params,Rule["tax_percent",ToString@N[#, {4, 3}] &@tax]]
		];
	];

	If[!(params === {}), body = Fold[#1 <> "&" <> #2 &, First[#] <> "=" <> Last[#] & /@ params], body = ""];
	
	raw = stripeimportdata@OAuthClient`rawoauthdata[id,"RawSubscriptionCreate",{"id"->customer,"ParameterlessBodyData"->body}];	
	If[!KeyExistsQ[#, "error"],
		(
			predata = Module[{unixrules, unixdiscount, unixplan, planamount},
											unixrules = Replace[#, {Rule["current_period_end", x_Integer] :> Rule["current_period_end", DateObject[AbsoluteTime[{1970, 1, 1}] + x, "TimeZone" -> 0]], Rule["start", y_Integer] :> Rule["start",DateObject[AbsoluteTime[{1970, 1, 1}] + y,"TimeZone" -> 0]], Rule["current_period_start", z_Integer] :> Rule["current_period_start", DateObject[AbsoluteTime[{1970, 1, 1}] + z, "TimeZone" -> 0]], Rule["canceled_at", w_Integer] :> Rule["canceled_at", DateObject[AbsoluteTime[{1970, 1, 1}] + w,"TimeZone" -> 0]],Rule["ended_at", u_Integer] :> Rule["ended_at", DateObject[AbsoluteTime[{1970, 1, 1}] + u, "TimeZone" -> 0]], Rule["trial_end", s_Integer] :> Rule["trial_end", DateObject[AbsoluteTime[{1970, 1, 1}] + s, "TimeZone" -> 0]], Rule["trial_start", t_Integer] :> Rule["trial_start", DateObject[AbsoluteTime[{1970, 1, 1}] + t, "TimeZone" -> 0]]}, {1}];
											unixdiscount = Replace[unixrules,{Rule["created", x_Integer] :> Rule["created", DateObject[AbsoluteTime[{1970, 1, 1}] + x, "TimeZone" -> 0]],Rule["start", y_Integer] :> Rule["start", DateObject[AbsoluteTime[{1970, 1, 1}] + y, "TimeZone" -> 0]],Rule["end", z_Integer] :> Rule["end", DateObject[AbsoluteTime[{1970, 1, 1}] + z, "TimeZone" -> 0]],Rule["redeem_by", w_Integer] :> Rule["redeem_by", DateObject[AbsoluteTime[{1970, 1, 1}] + w, "TimeZone" -> 0]],Rule["duration",n_String]:>Rule["duration",OAuthClient`Private`camelCase@n]},{3,5}];
											unixplan = Replace[unixdiscount,{List[a___Rule,Rule["amount_off",x_],Rule["currency",y_],b___Rule]:>List[a,If[x===Null,Rule["amount",Missing["NotAvailable"]],Rule["amount",If[MemberQ[$StripeNoCent,y],Quantity[x,y],NumberForm[N[#],{10,2},NumberPadding->{"","0"},ExponentFunction->(Null&)]&@Quantity[(x/100),y]]]],b]},{4}];
											planamount = Replace[unixplan, {List[a___, Rule["currency", x_], b___, Rule["amount", y_], c___] :> List[a, b, Rule["amount", If[MemberQ[$StripeNoCent, x], Quantity[y, x], NumberForm[N[#], {10, 2}, NumberPadding -> {"", "0"}, ExponentFunction -> (Null &)] &@Quantity[(y/100), x]]], c]}, {2}]]
		),
		errmsg = "message" /. ("error" /. raw);
		Message[ServiceExecute::apierr,errmsg];
		Throw[$Failed]
		]&@ raw;
	predata = Replace[predata, x_Rule /; First[x] === "metadata" :> Rule[First[x], placeholder[First[#]] -> Last[#] & /@ Last[x]], Infinity];
	predata = Replace[predata, {Rule[string_String, x_] :> Rule[OAuthClient`Private`camelCase@string, x]}, Infinity];
	data = Replace[predata, x_Rule /; Head@First[x] === placeholder :> Rule[First[First[x]], Last[x]], Infinity];
	data = data //. {Rule["Object", x_String] :> Rule["Object", OAuthClient`Private`camelCase@x], Rule["Status", y_String] :> Rule["Status", OAuthClient`Private`camelCase@y],Rule["Interval", z_String] :> Rule["Interval", OAuthClient`Private`camelCase@z],"Metadata":>MetaInformation};
	data = Replace[data,{Rule[rule_,Null]:>Rule[rule,Missing["NotAvailable"]]},Infinity];
	Dataset@Association[Replace[data, {r : {___Rule} :> Association[r]}, -1]]
]

stripecookeddata["UpdateSubscription",id_, args_?(MatchQ[#,{___Rule}]&)] :=  Block[{invalidParameters,params={},cparams={},customer,subscription,plan,coupon,trialend,metadata,payment,number,expdate,expmon,expyear,cvc,cardtoken,
												prorate,proratedate,quant,fee,tax,body,raw,predata,data,errmsg},

	invalidParameters = Select[Keys[args],!MemberQ[{"CustomerID","SubscriptionID","PlanID","CouponCode","Prorate","ProrationDate","TrialEndDate",MetaInformation,"PaymentMethod","Quantity","ApplicationFeeFraction","TaxFraction"},#]&]; 
	
	If[Length[invalidParameters]>0,
		(
			Message[ServiceObject::noget,#,"Stripe"]&/@invalidParameters;
			Throw[$Failed]
		)];

	If[KeyExistsQ[args,"CustomerID"],
		(
			customer = "CustomerID" /. args;
			If[!StringQ[customer],
				(
				Message[ServiceExecute::nval,"CustomerID","Stripe"];
				Throw[$Failed]
				)
			];
		),
		Message[ServiceExecute::nparam,"CustomerID","Stripe"];
		Throw[$Failed]
	];

	If[KeyExistsQ[args,"SubscriptionID"],
		(
			subscription = "SubscriptionID" /. args;
			If[!StringQ[subscription],
				(
				Message[ServiceExecute::nval,"SubscriptionID","Stripe"];
				Throw[$Failed]
				)
			];
		),
		Message[ServiceExecute::nparam,"SubscriptionID","Stripe"];
		Throw[$Failed]
	];

	If[KeyExistsQ[args,"PlanID"],
		(
			plan = "PlanID" /. args;
			If[!StringQ[plan],
				(
				Message[ServiceExecute::nval,"PlanID","Stripe"];
				Throw[$Failed]
				),
			params = Append[params,Rule["plan",plan]]
			];
		)
	];
	
	If[KeyExistsQ[args,"CouponCode"],
		coupon = "CouponCode"/.args;
		If[!StringQ[coupon],
			(
			Message[ServiceExecute::nval,"CouponCode","Stripe"];
			Throw[$Failed]
			),
		params = Append[params,Rule["coupon",coupon]]
		];
	];	

	If[KeyExistsQ[args,"Prorate"],
		(
		prorate = "Prorate"/.args;
		If[!BooleanQ[prorate],
			Message[ServiceExecute::nval,"Prorate","Stripe"];
			Throw[$Failed]
			];
		params = Append[params,Rule["prorate",ToLowerCase@ToString[prorate]]];
		),
		params = Append[params,Rule["prorate","true"]];
	];

	If[KeyExistsQ[args,"ProrationDate"],
		(
		proratedate = "ProrationDate"/.args;
			If[!DateObjectQ[proratedate],
				Message[ServiceExecute::nval,"ProrationDate","Stripe"];
				Throw[$Failed]
			];
		params = Append[params,Rule["proration_date",ToString[Round@AbsoluteTime[proratedate]-AbsoluteTime[{1970, 1, 1}]]]];
		)
	];

	If[KeyExistsQ[args,"TrialEndDate"],
		(
		trialend = "TrialEndDate"/.args;
			If[!DateObjectQ[trialend],
				Message[ServiceExecute::nval,"TrialEndDate","Stripe"];
				Throw[$Failed]
			];
		params = Append[params,Rule["trial_end",ToString[Round@AbsoluteTime[trialend]-AbsoluteTime[{1970, 1, 1}]]]];
		)
	];
	
	If[KeyExistsQ[args,MetaInformation],
		(
			metadata = MetaInformation/.args;
			If[!Or[AssociationQ[metadata], Head[metadata] === List],
				Message[ServiceExecute::nval,MetaInformation,"Stripe"];
				Throw[$Failed]
			];
			params = Join[params, (Rule["metadata[" <> First[#] <> "]",Last[#]]) & /@ Normal@metadata];
		)
	];
	
	If[KeyExistsQ[args,"PaymentMethod"],
					
		payment = "PaymentMethod"/.args;
		
		If[!AssociationQ[payment],
			Message[ServiceExecute::nval,"PaymentMethod","Stripe"];
			Throw[$Failed]
			];

		invalidParameters = Select[Keys[payment],!MemberQ[{"Number","ExpirationDate","CVC"},#]&];
	
		If[Length[invalidParameters]>0,
			(
				Message[ServiceObject::noget,#,"Stripe"]&/@invalidParameters;
				Throw[$Failed]
			)];			
					
		If[KeyExistsQ[payment,"Number"],
			(
			number = "Number"/.payment;
				If[!Or[StringQ[number],IntegerQ[number]],
					Message[ServiceExecute::nval,"Number","Stripe"];
					Throw[$Failed]
				];
			number = ToString@number;
			cparams = Append[cparams,Rule["card[number]",number]];
			),
			Message[ServiceExecute::mparam,"Number","Stripe"];
			Throw[$Failed]
			];
					
		If[KeyExistsQ[payment,"ExpirationDate"],
			(
			expdate = "ExpirationDate"/.payment;
				If[!DateObjectQ[expdate],
					Message[ServiceExecute::nval,"ExpirationDate","Stripe"];
					Throw[$Failed]
				];
			expyear = ToString@DateList[expdate][[1]];
			expmon = ToString@DateList[expdate][[2]];
			cparams = Append[cparams,Rule["card[exp_year]",expyear]];
			cparams = Append[cparams,Rule["card[exp_month]",expmon]];
			),
			Message[ServiceExecute::mparam,"ExpirationDate","Stripe"];
			Throw[$Failed]
		];
	
			If[KeyExistsQ[payment,"CVC"],
				(
				cvc = "CVC"/.payment;
					If[!Or[StringQ[cvc],IntegerQ[cvc]],
						Message[ServiceExecute::nval,"CVC","Stripe"];
						Throw[$Failed]
					];
				cvc = ToString@cvc;
				cparams = Append[cparams,Rule["card[cvc]",cvc]];
				)
		];
		cardtoken = stripeimportdata@OAuthClient`rawoauthdata[id,"RawGetCardKey",cparams];
		If[KeyExistsQ[cardtoken, "error"],
			errmsg = "message" /. ("error" /. cardtoken);
			Message[ServiceExecute::apierr,errmsg];
			Throw[$Failed]
			];
		params = Append[params,Rule["source","id"/.cardtoken]];
	];
	
	If[KeyExistsQ[args,"Quantity"],
		(
		quant = "Quantity"/.args;
			If[!(IntegerQ[quant] && quant>0),
				Message[ServiceExecute::nval,"Quantity","Stripe"];
				Throw[$Failed]
			];
		params = Append[params,Rule["quantity",ToString@quant]];
		)
	];	

	If[KeyExistsQ[args,"ApplicationFeeFraction"],
		fee = "ApplicationFeeFraction"/.args;
		If[!(MatchQ[#,x_ /; MemberQ[{Real, Integer, Rational}, Head[x]] && 100 > x >= 1 && (100 x == Round[100 x])]&@fee),
			(
			Message[ServiceExecute::nval,"ApplicationFeeFraction","Stripe"];
			Throw[$Failed]
			),
		params = Append[params,Rule["application_fee_percent",ToString@N[#, {4, 3}] &@fee]]
		];
	];

	If[KeyExistsQ[args,"TaxFraction"],
		tax = "TaxFraction"/.args;
		If[!(MatchQ[#,x_ /; MemberQ[{Real, Integer, Rational}, Head[x]] && 100 > x >= 1 && (100 x == Round[100 x])]&@tax),
			(
			Message[ServiceExecute::nval,"TaxFraction","Stripe"];
			Throw[$Failed]
			),
		params = Append[params,Rule["tax_percent",ToString@N[#, {4, 3}] &@tax]]
		];
	];

	If[!(params === {}), body = Fold[#1 <> "&" <> #2 &, First[#] <> "=" <> Last[#] & /@ params], body = ""];
	
	raw = stripeimportdata@OAuthClient`rawoauthdata[id,"RawSubscriptionUpdate",{"customer"->customer,"id"->subscription,"ParameterlessBodyData"->body}];
	If[!KeyExistsQ[#, "error"],
		(
			predata = Module[{unixrules, unixdiscount, unixplan, planamount},
											unixrules = Replace[#, {Rule["current_period_end", x_Integer] :> Rule["current_period_end", DateObject[AbsoluteTime[{1970, 1, 1}] + x, "TimeZone" -> 0]], Rule["start", y_Integer] :> Rule["start",DateObject[AbsoluteTime[{1970, 1, 1}] + y,"TimeZone" -> 0]], Rule["current_period_start", z_Integer] :> Rule["current_period_start", DateObject[AbsoluteTime[{1970, 1, 1}] + z, "TimeZone" -> 0]], Rule["canceled_at", w_Integer] :> Rule["canceled_at", DateObject[AbsoluteTime[{1970, 1, 1}] + w,"TimeZone" -> 0]],Rule["ended_at", u_Integer] :> Rule["ended_at", DateObject[AbsoluteTime[{1970, 1, 1}] + u, "TimeZone" -> 0]], Rule["trial_end", s_Integer] :> Rule["trial_end", DateObject[AbsoluteTime[{1970, 1, 1}] + s, "TimeZone" -> 0]], Rule["trial_start", t_Integer] :> Rule["trial_start", DateObject[AbsoluteTime[{1970, 1, 1}] + t, "TimeZone" -> 0]]}, {1}];
											unixdiscount = Replace[unixrules,{Rule["created", x_Integer] :> Rule["created", DateObject[AbsoluteTime[{1970, 1, 1}] + x, "TimeZone" -> 0]],Rule["start", y_Integer] :> Rule["start", DateObject[AbsoluteTime[{1970, 1, 1}] + y, "TimeZone" -> 0]],Rule["end", z_Integer] :> Rule["end", DateObject[AbsoluteTime[{1970, 1, 1}] + z, "TimeZone" -> 0]],Rule["redeem_by", w_Integer] :> Rule["redeem_by", DateObject[AbsoluteTime[{1970, 1, 1}] + w, "TimeZone" -> 0]],Rule["duration",n_String]:>Rule["duration",OAuthClient`Private`camelCase@n]},{3,5}];
											unixplan = Replace[unixdiscount,{List[a___Rule,Rule["amount_off",x_],Rule["currency",y_],b___Rule]:>List[a,If[x===Null,Rule["amount",Missing["NotAvailable"]],Rule["amount",If[MemberQ[$StripeNoCent,y],Quantity[x,y],NumberForm[N[#],{10,2},NumberPadding->{"","0"},ExponentFunction->(Null&)]&@Quantity[(x/100),y]]]],b]},{4}];
											planamount = Replace[unixplan, {List[a___, Rule["currency", x_], b___, Rule["amount", y_], c___] :> List[a, b, Rule["amount", If[MemberQ[$StripeNoCent, x], Quantity[y, x], NumberForm[N[#], {10, 2}, NumberPadding -> {"", "0"}, ExponentFunction -> (Null &)] &@Quantity[(y/100), x]]], c]}, {2}]]
		),
		errmsg = "message" /. ("error" /. raw);
		Message[ServiceExecute::apierr,errmsg];
		Throw[$Failed]
		]&@ raw;
	predata = Replace[predata, x_Rule /; First[x] === "metadata" :> Rule[First[x], placeholder[First[#]] -> Last[#] & /@ Last[x]], Infinity];
	predata = Replace[predata, {Rule[string_String, x_] :> Rule[OAuthClient`Private`camelCase@string, x]}, Infinity];
	data = Replace[predata, x_Rule /; Head@First[x] === placeholder :> Rule[First[First[x]], Last[x]], Infinity];
	data = data //. {Rule["Object", x_String] :> Rule["Object", OAuthClient`Private`camelCase@x], Rule["Status", y_String] :> Rule["Status", OAuthClient`Private`camelCase@y],Rule["Interval", z_String] :> Rule["Interval", OAuthClient`Private`camelCase@z],"Metadata":>MetaInformation};
	data = Replace[data,{Rule[rule_,Null]:>Rule[rule,Missing["NotAvailable"]]},Infinity];
	Dataset@Association[Replace[data, {r : {___Rule} :> Association[r]}, -1]]
]

stripecookeddata["RemoveSubscription",id_, args_?(MatchQ[#,{___Rule}]&)] :=  Block[{invalidParameters,params={},customer,subscription,delayed,
														raw,predata,data,errmsg},

	invalidParameters = Select[Keys[args],!MemberQ[{"CustomerID","SubscriptionID","Delayed"},#]&]; 
	
	If[Length[invalidParameters]>0,
		(
			Message[ServiceObject::noget,#,"Stripe"]&/@invalidParameters;
			Throw[$Failed]
		)];

	If[KeyExistsQ[args,"CustomerID"],
		(
			customer = "CustomerID" /. args;
			If[!StringQ[customer],
				(
				Message[ServiceExecute::nval,"CustomerID","Stripe"];
				Throw[$Failed]
				),
			params = Append[params,Rule["customer",customer]]
			];
		),
		Message[ServiceExecute::nparam,"CustomerID","Stripe"];
		Throw[$Failed]
	];

	If[KeyExistsQ[args,"SubscriptionID"],
		(
			subscription = "SubscriptionID" /. args;
			If[!StringQ[subscription],
				(
				Message[ServiceExecute::nval,"SubscriptionID","Stripe"];
				Throw[$Failed]
				),
			params = Append[params,Rule["id",subscription]]
			];
		),
		Message[ServiceExecute::nparam,"SubscriptionID","Stripe"];
		Throw[$Failed]
	];

	If[KeyExistsQ[args,"Delayed"],
		(
		delayed = "Delayed"/.args;
		If[!BooleanQ[delayed],
			Message[ServiceExecute::nval,"Delayed","Stripe"];
			Throw[$Failed]
			];
		params = Append[params,Rule["at_period_end",ToLowerCase@ToString[delayed]]];
		),
		params = Append[params,Rule["at_period_end","false"]];
	];

	raw = stripeimportdata@OAuthClient`rawoauthdata[id,"RawSubscriptionDelete",params];
	If[!KeyExistsQ[#, "error"],
		(
			predata = Module[{unixrules, unixdiscount, unixplan, planamount},
											unixrules = Replace[#, {Rule["current_period_end", x_Integer] :> Rule["current_period_end", DateObject[AbsoluteTime[{1970, 1, 1}] + x, "TimeZone" -> 0]], Rule["start", y_Integer] :> Rule["start",DateObject[AbsoluteTime[{1970, 1, 1}] + y,"TimeZone" -> 0]], Rule["current_period_start", z_Integer] :> Rule["current_period_start", DateObject[AbsoluteTime[{1970, 1, 1}] + z, "TimeZone" -> 0]], Rule["canceled_at", w_Integer] :> Rule["canceled_at", DateObject[AbsoluteTime[{1970, 1, 1}] + w,"TimeZone" -> 0]],Rule["ended_at", u_Integer] :> Rule["ended_at", DateObject[AbsoluteTime[{1970, 1, 1}] + u, "TimeZone" -> 0]], Rule["trial_end", s_Integer] :> Rule["trial_end", DateObject[AbsoluteTime[{1970, 1, 1}] + s, "TimeZone" -> 0]], Rule["trial_start", t_Integer] :> Rule["trial_start", DateObject[AbsoluteTime[{1970, 1, 1}] + t, "TimeZone" -> 0]]}, {1}];
											unixdiscount = Replace[unixrules,{Rule["created", x_Integer] :> Rule["created", DateObject[AbsoluteTime[{1970, 1, 1}] + x, "TimeZone" -> 0]],Rule["start", y_Integer] :> Rule["start", DateObject[AbsoluteTime[{1970, 1, 1}] + y, "TimeZone" -> 0]],Rule["end", z_Integer] :> Rule["end", DateObject[AbsoluteTime[{1970, 1, 1}] + z, "TimeZone" -> 0]],Rule["redeem_by", w_Integer] :> Rule["redeem_by", DateObject[AbsoluteTime[{1970, 1, 1}] + w, "TimeZone" -> 0]],Rule["duration",n_String]:>Rule["duration",OAuthClient`Private`camelCase@n]},{3,5}];
											unixplan = Replace[unixdiscount,{List[a___Rule,Rule["amount_off",x_],Rule["currency",y_],b___Rule]:>List[a,If[x===Null,Rule["amount",Missing["NotAvailable"]],Rule["amount",If[MemberQ[$StripeNoCent,y],Quantity[x,y],NumberForm[N[#],{10,2},NumberPadding->{"","0"},ExponentFunction->(Null&)]&@Quantity[(x/100),y]]]],b]},{4}];
											planamount = Replace[unixplan, {List[a___, Rule["currency", x_], b___, Rule["amount", y_], c___] :> List[a, b, Rule["amount", If[MemberQ[$StripeNoCent, x], Quantity[y, x], NumberForm[N[#], {10, 2}, NumberPadding -> {"", "0"}, ExponentFunction -> (Null &)] &@Quantity[(y/100), x]]], c]}, {2}]]
		),
		errmsg = "message" /. ("error" /. raw);
		Message[ServiceExecute::apierr,errmsg];
		Throw[$Failed]
		]&@ raw;
	predata = Replace[predata, x_Rule /; First[x] === "metadata" :> Rule[First[x], placeholder[First[#]] -> Last[#] & /@ Last[x]], Infinity];
	predata = Replace[predata, {Rule[string_String, x_] :> Rule[OAuthClient`Private`camelCase@string, x]}, Infinity];
	data = Replace[predata, x_Rule /; Head@First[x] === placeholder :> Rule[First[First[x]], Last[x]], Infinity];
	data = data //. {Rule["Object", x_String] :> Rule["Object", OAuthClient`Private`camelCase@x], Rule["Status", y_String] :> Rule["Status", OAuthClient`Private`camelCase@y],Rule["Interval", z_String] :> Rule["Interval", OAuthClient`Private`camelCase@z],"Metadata":>MetaInformation};
	data = Replace[data,{Rule[rule_,Null]:>Rule[rule,Missing["NotAvailable"]]},Infinity];
	Dataset@Association[Replace[data, {r : {___Rule} :> Association[r]}, -1]]
]

stripecookeddata["ListPlans",id_, args_?(MatchQ[#,{___Rule}]&)] :=  Block[{invalidParameters,params={},plan,window,limit,raw,predata,data,errmsg},

	invalidParameters = Select[Keys[args],!MemberQ[{"PlanID","DateWindow","MaxItems"},#]&]; 
	
	If[Length[invalidParameters]>0,
		(
			Message[ServiceObject::noget,#,"Stripe"]&/@invalidParameters;
			Throw[$Failed]
		)];
		
	Which[
				KeyExistsQ[args,"PlanID"] && !Fold[Or[#1, #2] &, (KeyExistsQ[args, #] &) /@ {"DateWindow","MaxItems"}],
					plan = "PlanID" /. args;
					If[!StringQ[plan],
						(
						Message[ServiceExecute::nval,"PlanID","Stripe"];
						Throw[$Failed]
						),
					params = Append[params,Rule["id",plan]];
					];
					
					raw = stripeimportdata@OAuthClient`rawoauthdata[id,"RawPlanRetrieve",params];
					If[!KeyExistsQ[#, "error"],
						(
							predata = Module[{unixplan, planamount},
								unixplan = Replace[#, {Rule["created", z_Integer] :> Rule["created", DateObject[AbsoluteTime[{1970, 1, 1}] + z, "TimeZone" -> 0]]}, {1}];
								planamount = Replace[unixplan, {List[a___, Rule["currency", x_], b___, Rule["amount", y_], c___] :> List[a, b, Rule["amount", If[MemberQ[$StripeNoCent, x], Quantity[y, x], NumberForm[N[#], {10, 2}, NumberPadding -> {"", "0"}, ExponentFunction -> (Null &)] &@Quantity[(y/100), x]]], c]}, {0}]]
						),
						errmsg = "message" /. ("error" /. raw);
						Message[ServiceExecute::apierr,errmsg];
						Throw[$Failed]
						]&@ raw;
					predata = Replace[predata, x_Rule /; First[x] === "metadata" :> Rule[First[x], placeholder[First[#]] -> Last[#] & /@ Last[x]], Infinity];
					predata = Replace[predata, {Rule[string_String, x_] :> Rule[OAuthClient`Private`camelCase@string, x]}, Infinity];
					data = Replace[predata, x_Rule /; Head@First[x] === placeholder :> Rule[First[First[x]], Last[x]], Infinity];
					data = data //. {Rule["Object", x_String] :> Rule["Object", OAuthClient`Private`camelCase@x], Rule["Interval", y_String] :> Rule["Interval", OAuthClient`Private`camelCase@y],"Metadata":>MetaInformation};
					data = Replace[data,{Rule[rule_,Null]:>Rule[rule,Missing["NotAvailable"]]},Infinity];
					Dataset@Association[Replace[data, {r : {___Rule} :> Association[r]}, -1]]

				,
					
				KeyExistsQ[args,"PlanID"] && Fold[Or[#1, #2] &, (KeyExistsQ[args, #] &) /@ {"DateWindow","MaxItems"}],
					Message[ServiceExecute::invid,"ListPlans","PlanID"];
					Throw[$Failed]	
					
				,
				
				!KeyExistsQ[args,"PlanID"] && Fold[Or[#1, #2] &, (KeyExistsQ[args, #] &) /@ {"DateWindow","MaxItems"}],
				
					If[KeyExistsQ[args,"DateWindow"],
						(
						window = "DateWindow"/.args;
							If[!(Head[window] === List && Length[window] === 2 && Fold[And, (DateObjectQ /@ window)]),
								Message[ServiceExecute::nval,"DateWindow","Stripe"];
								Throw[$Failed]
							];
						params = Append[params,Rule["created[gte]",ToString[Round@AbsoluteTime[window[[1]]]-AbsoluteTime[{1970, 1, 1}]]]];
						params = Append[params,Rule["created[lte]",ToString[Round@AbsoluteTime[window[[2]]]-AbsoluteTime[{1970, 1, 1}]]]];
						)
					];
					
					If[KeyExistsQ[args,"MaxItems"],
						(
						limit = "MaxItems"/.args;
							If[!(IntegerQ[limit] && limit>0),
								Message[ServiceExecute::nval,"MaxItems","Stripe"];
								Throw[$Failed]
							];
						params = Append[params,Rule["limit",ToString[limit]]];
						)
					];
					
					raw = stripeimportdata@OAuthClient`rawoauthdata[id,"RawPlanList",params];
					If[KeyExistsQ[raw, "error"],
						errmsg = "message" /. ("error" /. raw);
						Message[ServiceExecute::apierr,errmsg];
						Throw[$Failed],
						(
							predata = Module[{unixplan, planamount},
								unixplan = Replace[#, {Rule["created", z_Integer] :> Rule["created", DateObject[AbsoluteTime[{1970, 1, 1}] + z, "TimeZone" -> 0]]}, {1}];
								planamount = Replace[unixplan, {List[a___, Rule["currency", x_], b___, Rule["amount", y_], c___] :> List[a, b, Rule["amount", If[MemberQ[$StripeNoCent, x], Quantity[y, x], NumberForm[N[#], {10, 2}, NumberPadding -> {"", "0"}, ExponentFunction -> (Null &)] &@Quantity[(y/100), x]]], c]}, {0}]
							]& /@ ("data" /.raw);
					
						predata = Replace[predata, x_Rule /; First[x] === "metadata" :> Rule[First[x], placeholder[First[#]] -> Last[#] & /@ Last[x]], Infinity];
						predata = Replace[predata, {Rule[string_String, x_] :> Rule[OAuthClient`Private`camelCase@string, x]}, Infinity];
						data = Replace[predata, x_Rule /; Head@First[x] === placeholder :> Rule[First[First[x]], Last[x]], Infinity];
						data = data //. {Rule["Object", x_String] :> Rule["Object", OAuthClient`Private`camelCase@x], Rule["Interval", y_String] :> Rule["Interval", OAuthClient`Private`camelCase@y],"Metadata":>MetaInformation};
						data = Replace[data,{Rule[rule_,Null]:>Rule[rule,Missing["NotAvailable"]]},Infinity];
						Dataset[Association[Replace[#, {r : {___Rule} :> Association[r]}, -1]] & /@ data]
						)]
					
				,
				
				True,
					raw = stripeimportdata@OAuthClient`rawoauthdata[id,"RawPlanList",{}];
					If[KeyExistsQ[raw, "error"],
						errmsg = "message" /. ("error" /. raw);
						Message[ServiceExecute::apierr,errmsg];
						Throw[$Failed],
						(
							predata = Module[{unixplan, planamount},
								unixplan = Replace[#, {Rule["created", z_Integer] :> Rule["created", DateObject[AbsoluteTime[{1970, 1, 1}] + z, "TimeZone" -> 0]]}, {1}];
								planamount = Replace[unixplan, {List[a___, Rule["currency", x_], b___, Rule["amount", y_], c___] :> List[a, b, Rule["amount", If[MemberQ[$StripeNoCent, x], Quantity[y, x], NumberForm[N[#], {10, 2}, NumberPadding -> {"", "0"}, ExponentFunction -> (Null &)] &@Quantity[(y/100), x]]], c]}, {0}]
							]& /@ ("data" /.raw);
					
						predata = Replace[predata, x_Rule /; First[x] === "metadata" :> Rule[First[x], placeholder[First[#]] -> Last[#] & /@ Last[x]], Infinity];
						predata = Replace[predata, {Rule[string_String, x_] :> Rule[OAuthClient`Private`camelCase@string, x]}, Infinity];
						data = Replace[predata, x_Rule /; Head@First[x] === placeholder :> Rule[First[First[x]], Last[x]], Infinity];
						data = data //. {Rule["Object", x_String] :> Rule["Object", OAuthClient`Private`camelCase@x], Rule["Interval", y_String] :> Rule["Interval", OAuthClient`Private`camelCase@y],"Metadata":>MetaInformation};
						data = Replace[data,{Rule[rule_,Null]:>Rule[rule,Missing["NotAvailable"]]},Infinity];
						Dataset[Association[Replace[#, {r : {___Rule} :> Association[r]}, -1]] & /@ data]
						)]
     	]			
]

stripecookeddata["AddPlan",id_, args_?(MatchQ[#,{___Rule}]&)] :=  Block[{invalidParameters,params={},plan,plantext,quant,amount,currency,billing,trial,metadata,statement,
												body,raw,predata,data,errmsg},

	invalidParameters = Select[Keys[args],!MemberQ[{"PlanID","PlanText","Amount","BillingInterval","TrialPeriod",MetaInformation,"StatementText"},#]&]; 
	
	If[Length[invalidParameters]>0,
		(
			Message[ServiceObject::noget,#,"Stripe"]&/@invalidParameters;
			Throw[$Failed]
		)];

	If[KeyExistsQ[args,"PlanID"],
		(
			plan = "PlanID" /. args;
			If[!StringQ[plan],
				(
				Message[ServiceExecute::nval,"PlanID","Stripe"];
				Throw[$Failed]
				),
			params = Append[params,Rule["id",plan]]
			];
		),
		Message[ServiceExecute::nparam,"PlanID","Stripe"];
		Throw[$Failed]
	];
	
	If[KeyExistsQ[args,"PlanText"],
		(
			plantext = "PlanText"/.args;
			If[!StringQ[plantext],
				(
				Message[ServiceExecute::nval,"PlanText","Stripe"];
				Throw[$Failed]
				),
			params = Append[params,Rule["name",plantext]]
			];
		),
		Message[ServiceExecute::nparam,"PlanText","Stripe"];
		Throw[$Failed]		
	];	

	If[KeyExistsQ[args,"Amount"],
		(
		quant = "Amount"/.args;
			If[!QuantityQ[quant],
				Message[ServiceExecute::nval,"Amount","Stripe"];
				Throw[$Failed]
			];
		currency = QuantityUnit[quant]/.$StripeCurrencyToCode;
		amount = QuantityMagnitude[quant];
			If[!MemberQ[$StripeNoCent,currency],
			amount = 100*amount;
			];
			If[Or[!MemberQ[$StripeCurrencies,currency],!MatchQ[amount, x_ /; MemberQ[{Real, Integer}, Head[x]] && (x == Round[x]) && x>=0]],
				Message[ServiceExecute::nval,"Amount","Stripe"];
				Throw[$Failed]
			];		
		params = Append[params,Rule["amount",ToString@Round@amount]];
		params = Append[params,Rule["currency",currency]];
		),
		Message[ServiceExecute::nparam,"Amount","Stripe"];
		Throw[$Failed]			
	];
	
	If[KeyExistsQ[args,"BillingInterval"],
		(
		billing = "BillingInterval"/.args;
			If[!(Head[billing] === List && Length[billing] === 2 && IntegerQ[First@billing] && MemberQ[{"Day","Week","Month","Year"},Last@billing]),
				Message[ServiceExecute::nval,"BillingInterval","Stripe"];
				Throw[$Failed]
			];
		params = Append[params,Rule["interval",ToLowerCase@Last@billing]];
		params = Append[params,Rule["interval_count",ToString@First@billing]];
		),
		Message[ServiceExecute::nparam,"BillingInterval","Stripe"];
		Throw[$Failed]		
	];		

	If[KeyExistsQ[args,"TrialPeriod"],
		(
		trial = "TrialPeriod"/.args;
		If[!(QuantityQ[trial] && QuantityUnit@trial === "Days" && IntegerQ[QuantityMagnitude[trial]]),
				Message[ServiceExecute::nval,"TrialPeriod","Stripe"];
				Throw[$Failed]
			];
		params = Append[params,Rule["trial_period_days",ToString@QuantityMagnitude[trial]]];
		)
	];
	
	If[KeyExistsQ[args,MetaInformation],
		(
			metadata = MetaInformation/.args;
			If[!Or[AssociationQ[metadata], Head[metadata] === List],
				Message[ServiceExecute::nval,MetaInformation,"Stripe"];
				Throw[$Failed]
			];
			params = Join[params, (Rule["metadata[" <> First[#] <> "]",Last[#]]) & /@ Normal@metadata];
		)
	];

	If[KeyExistsQ[args,"StatementText"],
		(
		statement = "StatementText"/.args;
		If[!StringQ[statement],
				Message[ServiceExecute::nval,"StatementText","Stripe"];
				Throw[$Failed]
			];
		params = Append[params,Rule["statement_descriptor",statement]];
		)
	];

	If[!(params === {}), body = Fold[#1 <> "&" <> #2 &, First[#] <> "=" <> Last[#] & /@ params], body = ""];
	
	raw = stripeimportdata@OAuthClient`rawoauthdata[id,"RawPlanCreate",{"ParameterlessBodyData"->body}];
	If[!KeyExistsQ[#, "error"],
		(
			predata = Module[{unixplan, planamount},
				unixplan = Replace[#, {Rule["created", z_Integer] :> Rule["created", DateObject[AbsoluteTime[{1970, 1, 1}] + z, "TimeZone" -> 0]]}, {1}];
				planamount = Replace[unixplan, {List[a___, Rule["currency", x_], b___, Rule["amount", y_], c___] :> List[a, b, Rule["amount", If[MemberQ[$StripeNoCent, x], Quantity[y, x], NumberForm[N[#], {10, 2}, NumberPadding -> {"", "0"}, ExponentFunction -> (Null &)] &@Quantity[(y/100), x]]], c]}, {0}]]
		),
		errmsg = "message" /. ("error" /. raw);
		Message[ServiceExecute::apierr,errmsg];
		Throw[$Failed]
		]&@ raw;
	predata = Replace[predata, x_Rule /; First[x] === "metadata" :> Rule[First[x], placeholder[First[#]] -> Last[#] & /@ Last[x]], Infinity];
	predata = Replace[predata, {Rule[string_String, x_] :> Rule[OAuthClient`Private`camelCase@string, x]}, Infinity];
	data = Replace[predata, x_Rule /; Head@First[x] === placeholder :> Rule[First[First[x]], Last[x]], Infinity];
	data = data //. {Rule["Object", x_String] :> Rule["Object", OAuthClient`Private`camelCase@x], Rule["Interval", y_String] :> Rule["Interval", OAuthClient`Private`camelCase@y],"Metadata":>MetaInformation};
	data = Replace[data,{Rule[rule_,Null]:>Rule[rule,Missing["NotAvailable"]]},Infinity];
	Dataset@Association[Replace[data, {r : {___Rule} :> Association[r]}, -1]]
]

stripecookeddata["UpdatePlan",id_, args_?(MatchQ[#,{___Rule}]&)] :=  Block[{invalidParameters,params={},plan,plantext,metadata,statement,
												body,raw,predata,data,errmsg},

	invalidParameters = Select[Keys[args],!MemberQ[{"PlanID","PlanText","StatementText",MetaInformation},#]&]; 
	
	If[Length[invalidParameters]>0,
		(
			Message[ServiceObject::noget,#,"Stripe"]&/@invalidParameters;
			Throw[$Failed]
		)];

	If[KeyExistsQ[args,"PlanID"],
		(
			plan = "PlanID" /. args;
			If[!StringQ[plan],
				(
				Message[ServiceExecute::nval,"PlanID","Stripe"];
				Throw[$Failed]
				)
			];
		),
		Message[ServiceExecute::nparam,"PlanID","Stripe"];
		Throw[$Failed]
	];
	
	If[KeyExistsQ[args,"PlanText"],
		(
			plantext = "PlanText"/.args;
			If[!StringQ[plantext],
				(
				Message[ServiceExecute::nval,"PlanText","Stripe"];
				Throw[$Failed]
				),
			params = Append[params,Rule["name",plantext]]
			];
		)
	];		
	
	If[KeyExistsQ[args,"StatementText"],
		(
		statement = "StatementText"/.args;
		If[!StringQ[statement],
				Message[ServiceExecute::nval,"StatementText","Stripe"];
				Throw[$Failed]
			];
		params = Append[params,Rule["statement_descriptor",statement]];
		)
	];	

	
	If[KeyExistsQ[args,MetaInformation],
		(
			metadata = MetaInformation/.args;
			If[!Or[AssociationQ[metadata], Head[metadata] === List],
				Message[ServiceExecute::nval,MetaInformation,"Stripe"];
				Throw[$Failed]
			];
			params = Join[params, (Rule["metadata[" <> First[#] <> "]",Last[#]]) & /@ Normal@metadata];
		)
	];

	If[!(params === {}), body = Fold[#1 <> "&" <> #2 &, First[#] <> "=" <> Last[#] & /@ params], body = ""];
	
	raw = stripeimportdata@OAuthClient`rawoauthdata[id,"RawPlanUpdate",{"id"->plan,"ParameterlessBodyData"->body}];
	If[!KeyExistsQ[#, "error"],
		(
			predata = Module[{unixplan, planamount},
				unixplan = Replace[#, {Rule["created", z_Integer] :> Rule["created", DateObject[AbsoluteTime[{1970, 1, 1}] + z, "TimeZone" -> 0]]}, {1}];
				planamount = Replace[unixplan, {List[a___, Rule["currency", x_], b___, Rule["amount", y_], c___] :> List[a, b, Rule["amount", If[MemberQ[$StripeNoCent, x], Quantity[y, x], NumberForm[N[#], {10, 2}, NumberPadding -> {"", "0"}, ExponentFunction -> (Null &)] &@Quantity[(y/100), x]]], c]}, {0}]]
		),
		errmsg = "message" /. ("error" /. raw);
		Message[ServiceExecute::apierr,errmsg];
		Throw[$Failed]
		]&@ raw;
	predata = Replace[predata, x_Rule /; First[x] === "metadata" :> Rule[First[x], placeholder[First[#]] -> Last[#] & /@ Last[x]], Infinity];
	predata = Replace[predata, {Rule[string_String, x_] :> Rule[OAuthClient`Private`camelCase@string, x]}, Infinity];
	data = Replace[predata, x_Rule /; Head@First[x] === placeholder :> Rule[First[First[x]], Last[x]], Infinity];
	data = data //. {Rule["Object", x_String] :> Rule["Object", OAuthClient`Private`camelCase@x], Rule["Interval", y_String] :> Rule["Interval", OAuthClient`Private`camelCase@y],"Metadata":>MetaInformation};
	data = Replace[data,{Rule[rule_,Null]:>Rule[rule,Missing["NotAvailable"]]},Infinity];
	Dataset@Association[Replace[data, {r : {___Rule} :> Association[r]}, -1]]
]

stripecookeddata["RemovePlan",id_, args_?(MatchQ[#,{___Rule}]&)] :=  Block[{invalidParameters,params={},plan,raw,deleted,data,errmsg},

	invalidParameters = Select[Keys[args],!MemberQ[{"PlanID"},#]&]; 
	
	If[Length[invalidParameters]>0,
		(
			Message[ServiceObject::noget,#,"Stripe"]&/@invalidParameters;
			Throw[$Failed]
		)];
		
	If[KeyExistsQ[args,"PlanID"],
		(
			plan = "PlanID" /. args;
			If[!StringQ[plan],
				(
				Message[ServiceExecute::nval,"PlanID","Stripe"];
				Throw[$Failed]
				),
			raw = stripeimportdata@OAuthClient`rawoauthdata[id,"RawPlanDelete",{"id"->plan}];
			If[KeyExistsQ[raw, "error"],
				(
				errmsg = "message" /. ("error" /. raw);
				Message[ServiceExecute::apierr,errmsg];
				Throw[$Failed]
				)];
			deleted = "deleted"/.raw;
			data = "id" /. raw;
			If[deleted,data]
			]
		),
		Message[ServiceExecute::nparam,"PlanID","Stripe"];
		Throw[$Failed]
	]
]

stripecookeddata["ListBalance",id_, args_?(MatchQ[#,{___Rule}]&)] :=  Block[{invalidParameters,
												raw,predata,data},

	invalidParameters = Select[Keys[args],!MemberQ[{},#]&]; 
	
	If[Length[invalidParameters]>0,
		(
			Message[ServiceObject::noget,#,"Stripe"]&/@invalidParameters;
			Throw[$Failed]
		)];

	raw = stripeimportdata@OAuthClient`rawoauthdata[id,"RawBalanceRetrieve",{}];
	predata = Replace[#, List["amount" -> x_, "currency" -> y_] :> If[MemberQ[$StripeNoCent, y], Quantity[x, y], NumberForm[N[#], {10, 2}, NumberPadding -> {"", "0"}, ExponentFunction -> (Null &)] &@ Quantity[(x/100), y]],{3}] &@raw;
	data = Replace[predata, {Rule[string_String, x_] :> Rule[OAuthClient`Private`camelCase@string, x]}, Infinity];
	Dataset@Association[Replace[#[[1 ;; 2]], {r : {___Rule} :> Association[r]}, -1]] &@ data
]

stripecookeddata["ListBalanceTransaction",id_, args_?(MatchQ[#,{___Rule}]&)] :=  Block[{invalidParameters,transaction,
												raw,predata,data,errmsg},

	invalidParameters = Select[Keys[args],!MemberQ[{"TransactionID"},#]&]; 
	
	If[Length[invalidParameters]>0,
		(
			Message[ServiceObject::noget,#,"Stripe"]&/@invalidParameters;
			Throw[$Failed]
		)];

	If[KeyExistsQ[args,"TransactionID"],
		(
			transaction = "TransactionID" /. args;
			If[!StringQ[transaction],
				(
				Message[ServiceExecute::nval,"TransactionID","Stripe"];
				Throw[$Failed]
				)
			];
		),
		Message[ServiceExecute::nparam,"TransactionID","Stripe"];
		Throw[$Failed]
	];
	
	raw = stripeimportdata@OAuthClient`rawoauthdata[id,"RawBalanceRetrieveTransaction",{"id"->transaction}];
	If[!KeyExistsQ[#, "error"],
		(
			predata = Module[{unixrules, amount,feeobj,stransf},
				unixrules = Replace[#, {Rule["created", x_Integer] :> Rule["created", DateObject[AbsoluteTime[{1970, 1, 1}] + x, "TimeZone" -> 0]],Rule["available_on", y_Integer] :> Rule["available_on", DateObject[AbsoluteTime[{1970, 1, 1}] + y, "TimeZone" -> 0]]}, {1}];
				amount = Replace[unixrules, {List[Rule["net", n_],a_, Rule["currency", y_], b___, Rule["amount", x_], c___,Rule["fee", z_],d___] :> List[Rule["net", If[MemberQ[$StripeNoCent, y], Quantity[n, y], NumberForm[N[#], {10, 2}, NumberPadding -> {"", "0"}, ExponentFunction -> (Null &)] &@Quantity[(n/100), y]]], a, Rule["amount", If[MemberQ[$StripeNoCent, y], Quantity[x, y], NumberForm[N[#], {10, 2}, NumberPadding -> {"", "0"}, ExponentFunction -> (Null &)] &@Quantity[(x/100), y]]],Rule["fee", If[MemberQ[$StripeNoCent, y], Quantity[z, y], NumberForm[N[#], {10, 2}, NumberPadding -> {"", "0"}, ExponentFunction -> (Null &)] &@Quantity[(z/100), y]]],b,c,d]}, {0}];
				feeobj = Replace[amount,Rule["fee_details",fees_]:>Rule["fee_details",Module[{feeP},feeP =Replace[#,{List[Rule["amount", x_],Rule["currency", y_],a___] :> List[Rule["amount", If[MemberQ[$StripeNoCent, y], Quantity[x, y], NumberForm[N[#], {10, 2}, NumberPadding -> {"", "0"}, ExponentFunction -> (Null &)] &@Quantity[(x/100), y]]],a]}]]&/@fees],{1}];
				stransf = Replace[feeobj,{Rule["sourced_transfers",st_]:>Rule["sourced_transfers", Module[{cardObj,unixrulez,revrsl,amounts},
					cardObj =Replace[#,{Rule["card",card_]:>Rule["card",Module[{quant,countryp,cvc},
						quant=Replace[#,List[a___,"exp_month"->x_,b___,"exp_year"->y_,c___]:>List[a,Rule["ExpirationDate",DateObject[{y,x}]],b,c],{0}];
						countryp=quant/.{Rule["country",x_String]:>Rule["country",Interpreter["Country"][x]]};
						cvc=countryp/.{"cvc_check":>"CVCCheck"};cvc = Replace[cvc,{Rule["CVCCheck",t_String]:>Rule["CVCCheck",OAuthClient`Private`camelCase@t],Rule["funding",t_String]:>Rule["funding",OAuthClient`Private`camelCase@t]}]]&@card]},{1}];
					unixrulez = Replace[cardObj,{Rule["created", x_Integer] :> Rule["created", DateObject[AbsoluteTime[{1970, 1, 1}] + x, "TimeZone" -> 0]],Rule["date", x_Integer] :> Rule["date", DateObject[AbsoluteTime[{1970, 1, 1}] + x, "TimeZone" -> 0]]},{1}];
					revrsl = Replace[unixrulez,Rule["reversals",rvs_]:>Rule["reversals",Module[{unix},
						unix = Replace[#, {Rule["created", x_Integer] :> Rule["created", DateObject[AbsoluteTime[{1970, 1, 1}] + x, "TimeZone" -> 0]]}, {1}];Replace[unix,{List[a___,Rule["amount", x_],b___,Rule["currency", y_],c___] :> List[a,Rule["amount", If[MemberQ[$StripeNoCent, y], Quantity[x, y], NumberForm[N[#], {10, 2}, NumberPadding -> {"", "0"}, ExponentFunction -> (Null &)] &@Quantity[(x/100), y]]],b,c]},{0}]]&/@("data"/.rvs)],{1}];
					amounts = Replace[revrsl,{List[a___,Rule["amount_reversed", z_],Rule["amount", x_],b___,Rule["currency", y_],c___] :> List[a,Rule["amount_reversed",If[MemberQ[$StripeNoCent, y], Quantity[z, y], NumberForm[N[#], {10, 2}, NumberPadding -> {"", "0"}, ExponentFunction -> (Null &)] &@Quantity[(z/100), y]]],Rule["amount", If[MemberQ[$StripeNoCent, y], Quantity[x, y], NumberForm[N[#], {10, 2}, NumberPadding -> {"", "0"}, ExponentFunction -> (Null &)] &@Quantity[(x/100), y]]],b,c]},{0}]]&/@("data"/.st)]},{1}]]
		),
		errmsg = "message" /. ("error" /. raw);
		Message[ServiceExecute::apierr,errmsg];
		Throw[$Failed]
		]&@ raw;
	predata = Replace[predata, x_Rule /; First[x] === "metadata" :> Rule[First[x], placeholder[First[#]] -> Last[#] & /@ Last[x]], Infinity];
	predata = Replace[predata, {Rule[string_String, x_] :> Rule[OAuthClient`Private`camelCase@string, x]}, Infinity];
	data = Replace[predata, x_Rule /; Head@First[x] === placeholder :> Rule[First[First[x]], Last[x]], Infinity];
	data = data //. {Rule["Object", x_String] :> Rule["Object", OAuthClient`Private`camelCase@x], Rule["Status", y_String] :> Rule["Status", OAuthClient`Private`camelCase@y],Rule["Type", x_String] :> Rule["Type", OAuthClient`Private`camelCase@x],Rule["FailureCode", x_String] :> Rule["FailureCode", OAuthClient`Private`camelCase@x],"Metadata":>MetaInformation};
	data = Replace[data,{Rule[rule_,Null]:>Rule[rule,Missing["NotAvailable"]]},Infinity];
	Dataset@Association[Replace[data, {r : {___Rule} :> Association[r]}, -1]]
]

stripecookeddata["ListBalanceHistory",id_, args_?(MatchQ[#,{___Rule}]&)] :=  Block[{invalidParameters,params={},limit,available,created,raw,predata,data,errmsg},

	invalidParameters = Select[Keys[args],!MemberQ[{"CreationDateWindow","AvailabilityDateWindow","MaxItems"},#]&]; 
	
	If[Length[invalidParameters]>0,
		(
			Message[ServiceObject::noget,#,"Stripe"]&/@invalidParameters;
			Throw[$Failed]
		)];

	If[KeyExistsQ[args,"AvailabilityDateWindow"],
		(
		available = "AvailabilityDateWindow"/.args;
			If[!(Head[available] === List && Length[available] === 2 && Fold[And, (DateObjectQ /@ available)]),
				Message[ServiceExecute::nval,"AvailabilityDateWindow","Stripe"];
				Throw[$Failed]
			];
		params = Append[params,Rule["available_on[gte]",ToString[Round@AbsoluteTime[available[[1]]]-AbsoluteTime[{1970, 1, 1}]]]];
		params = Append[params,Rule["available_on[lte]",ToString[Round@AbsoluteTime[available[[2]]]-AbsoluteTime[{1970, 1, 1}]]]];
		)
	];

	If[KeyExistsQ[args,"CreationDateWindow"],
		(
		created = "CreationDateWindow"/.args;
			If[!(Head[created] === List && Length[created] === 2 && Fold[And, (DateObjectQ /@ created)]),
				Message[ServiceExecute::nval,"CreationDateWindow","Stripe"];
				Throw[$Failed]
			];
		params = Append[params,Rule["created[gte]",ToString[Round@AbsoluteTime[created[[1]]]-AbsoluteTime[{1970, 1, 1}]]]];
		params = Append[params,Rule["created[lte]",ToString[Round@AbsoluteTime[created[[2]]]-AbsoluteTime[{1970, 1, 1}]]]];
		)
	];
	
	If[KeyExistsQ[args,"MaxItems"],
		(
		limit = "MaxItems"/.args;
			If[!(IntegerQ[limit] && limit>0),
				Message[ServiceExecute::nval,"MaxItems","Stripe"];
				Throw[$Failed]
			];
		params = Append[params,Rule["limit",ToString[limit]]];
		)
	];
						
	raw = stripeimportdata@OAuthClient`rawoauthdata[id,"RawBalanceHistoryList",params];
	If[KeyExistsQ[raw, "error"],
		errmsg = "message" /. ("error" /. raw);
		Message[ServiceExecute::apierr,errmsg];
		Throw[$Failed],
		(
		predata = Module[{unixrules, amount,feeobj,stransf},
			unixrules = Replace[#, {Rule["created", x_Integer] :> Rule["created", DateObject[AbsoluteTime[{1970, 1, 1}] + x, "TimeZone" -> 0]],Rule["available_on", y_Integer] :> Rule["available_on", DateObject[AbsoluteTime[{1970, 1, 1}] + y, "TimeZone" -> 0]]}, {1}];
			amount = Replace[unixrules, {List[Rule["net", n_],a_, Rule["currency", y_], b___, Rule["amount", x_], c___,Rule["fee", z_],d___] :> List[Rule["net", If[MemberQ[$StripeNoCent, y], Quantity[n, y], NumberForm[N[#], {10, 2}, NumberPadding -> {"", "0"}, ExponentFunction -> (Null &)] &@Quantity[(n/100), y]]], a, Rule["amount", If[MemberQ[$StripeNoCent, y], Quantity[x, y], NumberForm[N[#], {10, 2}, NumberPadding -> {"", "0"}, ExponentFunction -> (Null &)] &@Quantity[(x/100), y]]],Rule["fee", If[MemberQ[$StripeNoCent, y], Quantity[z, y], NumberForm[N[#], {10, 2}, NumberPadding -> {"", "0"}, ExponentFunction -> (Null &)] &@Quantity[(z/100), y]]],b,c,d]}, {0}];
			feeobj = Replace[amount,Rule["fee_details",fees_]:>Rule["fee_details",Module[{feeP},feeP =Replace[#,{List[Rule["amount", x_],Rule["currency", y_],a___] :> List[Rule["amount", If[MemberQ[$StripeNoCent, y], Quantity[x, y], NumberForm[N[#], {10, 2}, NumberPadding -> {"", "0"}, ExponentFunction -> (Null &)] &@Quantity[(x/100), y]]],a]}]]&/@fees],{1}];
			stransf = Replace[feeobj,{Rule["sourced_transfers",st_]:>Rule["sourced_transfers", Module[{cardObj,unixrulez,revrsl,amounts},
				cardObj =Replace[#,{Rule["card",card_]:>Rule["card",Module[{quant,countryp,cvc},
					quant=Replace[#,List[a___,"exp_month"->x_,b___,"exp_year"->y_,c___]:>List[a,Rule["ExpirationDate",DateObject[{y,x}]],b,c],{0}];
					countryp=quant/.{Rule["country",x_String]:>Rule["country",Interpreter["Country"][x]]};
					cvc=countryp/.{"cvc_check":>"CVCCheck"};cvc = Replace[cvc,{Rule["CVCCheck",t_String]:>Rule["CVCCheck",OAuthClient`Private`camelCase@t],Rule["funding",t_String]:>Rule["funding",OAuthClient`Private`camelCase@t]}]]&@card]},{1}];
				unixrulez = Replace[cardObj,{Rule["created", x_Integer] :> Rule["created", DateObject[AbsoluteTime[{1970, 1, 1}] + x, "TimeZone" -> 0]],Rule["date", x_Integer] :> Rule["date", DateObject[AbsoluteTime[{1970, 1, 1}] + x, "TimeZone" -> 0]]},{1}];
				revrsl = Replace[unixrulez,Rule["reversals",rvs_]:>Rule["reversals",Module[{unix},
					unix = Replace[#, {Rule["created", x_Integer] :> Rule["created", DateObject[AbsoluteTime[{1970, 1, 1}] + x, "TimeZone" -> 0]]}, {1}];Replace[unix,{List[a___,Rule["amount", x_],b___,Rule["currency", y_],c___] :> List[a,Rule["amount", If[MemberQ[$StripeNoCent, y], Quantity[x, y], NumberForm[N[#], {10, 2}, NumberPadding -> {"", "0"}, ExponentFunction -> (Null &)] &@Quantity[(x/100), y]]],b,c]},{0}]]&/@("data"/.rvs)],{1}];
				amounts = Replace[revrsl,{List[a___,Rule["amount_reversed", z_],Rule["amount", x_],b___,Rule["currency", y_],c___] :> List[a,Rule["amount_reversed",If[MemberQ[$StripeNoCent, y], Quantity[z, y], NumberForm[N[#], {10, 2}, NumberPadding -> {"", "0"}, ExponentFunction -> (Null &)] &@Quantity[(z/100), y]]],Rule["amount", If[MemberQ[$StripeNoCent, y], Quantity[x, y], NumberForm[N[#], {10, 2}, NumberPadding -> {"", "0"}, ExponentFunction -> (Null &)] &@Quantity[(x/100), y]]],b,c]},{0}]]&/@("data"/.st)]},{1}]
		]& /@ ("data" /.raw);
	
		predata = Replace[predata, x_Rule /; First[x] === "metadata" :> Rule[First[x], placeholder[First[#]] -> Last[#] & /@ Last[x]], Infinity];
		predata = Replace[predata, {Rule[string_String, x_] :> Rule[OAuthClient`Private`camelCase@string, x]}, Infinity];
		data = Replace[predata, x_Rule /; Head@First[x] === placeholder :> Rule[First[First[x]], Last[x]], Infinity];
		data = data //. {Rule["Object", x_String] :> Rule["Object", OAuthClient`Private`camelCase@x], Rule["Status", y_String] :> Rule["Status", OAuthClient`Private`camelCase@y],Rule["Type", x_String] :> Rule["Type", OAuthClient`Private`camelCase@x],Rule["FailureCode", x_String] :> Rule["FailureCode", OAuthClient`Private`camelCase@x],"Metadata":>MetaInformation};
		data = Replace[data,{Rule[rule_,Null]:>Rule[rule,Missing["NotAvailable"]]},Infinity];
		Dataset[Association[Replace[#, {r : {___Rule} :> Association[r]}, -1]] & /@ data]
		)]
]

stripecookeddata[___]:=$Failed

End[]

End[]

SetAttributes[{},{ReadProtected, Protected}];

System`Private`RestoreContextPath[];

(* Return three functions to define oauthservicedata, oauthcookeddata, oauthsendmessage  *)
{StripeOAuth`Private`stripedata,StripeOAuth`Private`stripecookeddata}
