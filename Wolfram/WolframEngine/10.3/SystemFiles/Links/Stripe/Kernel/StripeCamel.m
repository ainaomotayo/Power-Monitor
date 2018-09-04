(* Wolfram Language package *)

BeginPackage["StripeCamel`"];

$StripeShippingCamel::usage = "";
$StripePaymentMethodCamel::usage = "";
$StripeRefundCamel::usage = "";
$StripeDisputeCamel::usage = "";
$StripeDisputeEvidenceCamel::usage = "";
$StripeEvidenceDetailsCamel::usage = "";
$StripeChargeCamel::usage = "";
$StripeBankAccountCamel::usage = "";
$StripeTransferReversalCamel::usage = "";
$StripeTransferCamel::usage = "";
$StripeBalanceTransactionFeeCamel::usage = "";
$StripeBalanceTransactionCamel::usage = "";

Begin["`Private`"];

$StripeShippingCamel = {
	"address" -> "Address", "address_city" -> "City", "address_line1" -> "FirstLine",
	"address_line2" -> "SecondLine", "address_state" -> "State", "address_zip" -> "ZipCode",
	"adress_country" -> "Country", "carrier" -> "Carrier", "city" -> "City", "country" -> "Country",
	"line1" -> "FirstLine", "line2" -> "SecondLine", "name" -> "Name", "phone" -> "Phone",
	"postal_code" -> "PostalCode", "state" -> "State", "tracking_number" -> "TrackingNumber"
}

$StripePaymentMethodCamel = {
	"account" -> "Account", "address_city" -> "AddressCity", "address_country" -> "AddressCountry",
	"address_line1" -> "AddressLine1", "address_line1_check" -> "AddressLine1Check",
	"address_line2" -> "AddressLine2", "address_state" -> "AddressState", "address_zip" -> "AddressZip", 
	"address_zip_check" -> "AddressZipCheck", "android_pay" -> "AndroidPay", "apple_pay" -> "ApplePay", 
	"brand" -> "Brand", "card" -> "Card", "currency" -> "Currency", "country" -> "Country",
	"credit" -> "Credit", "customer" -> "Customer", "cvc_check" -> "CVCCheck", "debit" -> "Debit",
	"default_for_currency" -> "DefaultForCurrency", "dynamic_last4" -> "DynamicLast4", "exp_month" -> "ExpDate",
	"fail" -> "Fail", "fingerprint" -> "Fingerprint", "funding" -> "Funding", "id" -> "ID", "last4" -> "Last4",
	"metadata" -> MetaInformation, "name" -> "Name", "object" -> "Object", "pass" -> "Pass", "prepaid" -> "Prepaid",
	"recipient" -> "Recipient", "tokenization_method" -> "TokenizationMethod", "unavailable" -> Missing["NotAvailable"], 
	"unchecked" -> Missing["NotChecked"], "unknown" -> Missing["Unknown"], "Unknown" -> Missing["Unknown"], 
	Null -> Missing["NotAvailable"]
}

$StripeRefundCamel = {"balance_transaction" -> "BalanceTransaction", "object" -> "Object",
	"reason" -> "Reason", "receipt_number" -> "ReceiptNumber", "amount" -> "Amount", "id" -> "ID",
	"created" -> "Created", "refund" -> "Refund", "metadata" -> MetaInformation, "charge" -> "Charge",
	"duplicate" -> "Duplicate", "fraudulent" -> "Fraudulent", "requested_by_customer" -> "RequestedByCustomer",
	Null -> Missing["NotAvailable"]
}

$StripeDisputeCamel = {
	"amount" -> "Amount", "balance_transactions" -> "BalanceTransactions", "charge" -> "Charge",
	"charge_refunded" -> "ChargeRefunded", "created" -> "Created", "credit_not_processed" -> "CreditNotProcessed", 
	"dispute" -> "Dispute", "duplicate" -> "Duplicate", "evidence" -> "Evidence", "evidence_details" -> "EvidenceDetails",
	"fraudulent" -> "Fraudulent", "general" -> "General", "id" -> "ID", "is_charge_refundable" -> "IsChargeRefundable",
	"livemode" -> "Livemode", "lost" -> "Lost", "metadata" -> MetaInformation, "needs_response" -> "NeedsResponse",
	"object" -> "Object", "product_not_received" -> "ProductNotReceived", "product_unacceptable" -> "ProductUnacceptable",
	"reason" -> "Reason", "response_disabled" -> "ResponseDisabled", "status" -> "Status",
	"subscription_canceled" -> "SubscriptionCanceled", "under_review" -> "UnderReview",	"unrecognized" -> "Unrecognized",
	"warning_closed" -> "WarningClosed", "warning_needs_response" -> "WarningNeedsResponse",
	"warning_under_review" -> "WarningUnderReview", "won" -> "Won", Null -> Missing["NotAvailable"]}

$StripeDisputeEvidenceCamel = {
	"access_activity_log" -> "AccessActivityLog", "billing_address" -> "BillingAddress", 
	"cancellation_policy" -> "CancellationPolicy", "cancellation_policy_disclosure" -> "CancellationPolicyDisclosure",
	"cancellation_rebuttal" -> "CancellationRebuttal", "customer_communication" -> "CustomerCommunication", 
	"customer_email_address" -> "CustomerEmailAddress", "customer_name" -> "CustomerName", 
	"customer_purchase_ip" -> "CustomerPurchaseIp", "customer_signature" -> "CustomerSignature", 
	"duplicate_charge_documentation" -> "DuplicateChargeDocumentation",
	"duplicate_charge_explanation" -> "DuplicateChargeExplanation", 
	"duplicate_charge_id" -> "DuplicateChargeID", "product_description" -> "ProductDescription", 
	Null -> Missing["NotAvailable"], "receipt" -> "Receipt", "refund_policy" -> "RefundPolicy",
	"refund_policy_disclosure" -> "RefundPolicyDisclosure", "refund_refusal_explanation" -> "RefundRefusalExplanation",
	"service_date" -> "ServiceDate", "service_documentation" -> "ServiceDocumentation", "shipping_address" -> "ShippingAddress", 
	"shipping_carrier" -> "ShippingCarrier", "shipping_date" -> "ShippingDate", 
	"shipping_documentation" -> "ShippingDocumentation", "shipping_tracking_number" -> "ShippingTrackingNumber", 
	"uncategorized_file" -> "UncategorizedFile", "uncategorized_text" -> "UncategorizedText"}

$StripeEvidenceDetailsCamel = {"due_by" -> "DueBy", 
	"has_evidence" -> "HasEvidence", "past_due" -> "PastDue", 
	"submission_count" -> "SubmissionCount"}

$StripeChargeCamel = {"amount" -> "Amount", "amount_refunded" -> "AmountRefunded", "application_fee" -> "ApplicationFee",
	"balance_transaction" -> "BalanceTransaction", "captured" -> "Captured", "charge" -> "Charge", "created" -> "Created",
	"customer" -> "Customer", "description" -> "Description", "destination" -> "Destination", "dispute" -> "Dispute",
	"failed" -> "Failed", "failure_code" -> "FailureCode", "failure_message" -> "FailureMessage",
	"fraud_details" -> "FraudDetails", "id" -> "ID", "invoice" -> "Invoice", "livemode" -> "Livemode",
	"metadata" -> MetaInformation, Null -> Missing["NotAvailable"], "object" -> "Object", "paid" -> "Paid", 
	"receipt_email" -> "ReceiptEmail", "receipt_number" -> "ReceiptNumber", "refunded" -> "Refunded", 
	"refunds" -> "Refunds", "shipping" -> "Shipping", "source" -> "Source",	"statement_descriptor" -> "StatementDescriptor",
	"status" -> "Status", "succeeded" -> "Succeeded", "transfer" -> "Transfer"};

$StripeBankAccountCamel = {
	"account" -> "Account", "bank_account" -> "BankAccount", "bank_name" -> "BankName", "country" -> "Country",
	"currency" -> "Currency", "default_for_currency" -> "DefaultForCurrency", "errored" -> "Errored",
	"fingerprint" -> "Fingerprint", "id" -> "ID", "last4" -> "Last4", "metadata" -> MetaInformation, "new" -> "New",
	"object" -> "Object", "routing_number" -> "RoutingNumber", "status" -> "Status", "validated" -> "Validated",
	"verified" -> "Verified", Null -> Missing["NotAvailable"]
}

$StripeTransferReversalCamel = {
	"amount"->"Amount", "balance_transaction"->"BalanceTransaction", "created"->"Created",
	"id"->"ID","metadata"->MetaInformation, "object"->"Object", "transfer"->"Transfer",
	"transfer_reversal"->"TransferReversal",Null -> Missing["NotAvailable"]
}

$StripeTransferCamel = {
	"account_closed" -> "AccountClosed", "account_frozen" -> "AccountFrozen",
	"amount" -> "Amount", "amount_reversed" -> "AmountReversed", 
	"application_fee" -> "ApplicationFee", "balance_transaction" -> "BalanceTransaction", 
	"bank_account" -> "BankAccount", "bank_account_restricted" -> "BankAccountRestricted", 
	"bank_ownership_changed" -> "BankOwnershipChanged", "canceled" -> "Canceled",
	"card" -> "Card", "could_not_process" -> "CouldNotProcess", "created" -> "Created", 
	"date" -> "Date", "debit_not_authorized" -> "DebitNotAuthorized", 
	"description" -> "Description", "destination" -> "Destination", 
	"destination_payment" -> "DestinationPayment", "failed" -> "Failed",
	"failure_code" -> "FailureCode", "failure_message" -> "FailureMessage", "id" -> "ID", 
	"insufficient_funds" -> "InsufficientFunds", "in_transit" -> "InTransit", 
	"invalid_account_number" -> "InvalidAccountNumber",
	"invalid_currency" -> "InvalidCurrency", "livemode" -> "Livemode", 
	"metadata" -> MetaInformation, "no_account" -> "NoAccount", 
	"object" -> "Object", "paid" -> "Paid", "pending" -> "Pending", 
	"recipient" -> "Recipient", "reversals" -> "Reversals", "reversed" -> "Reversed", 
	"source_transaction" -> "SourceTransaction", "statement_descriptor" -> "StatementDescriptor", 
	"status" -> "Status", "stripe_account" -> "StripeAccount", "transfer" -> "Transfer",
	"type" -> "Type", Null -> Missing["NotAvailable"]
}

$StripeBalanceTransactionCamel = {
	"adjustment" -> "Adjustment", "amount" -> "Amount", "application_fee" -> "ApplicationFee", "application_fee_refund" -> "ApplicationFeeRefund", 
	"available" -> "Available", "available_on" -> "AvailableOn", "balance_transaction" -> "BalanceTransaction", "charge" -> "Charge", "created" -> "Created", 
	"description" -> "Description", "fee" -> "Fee", "fee_details" -> "FeeDetails", "id" -> "ID", "net" -> "Net", "object" -> "Object", "pending" -> "Pending",
	"refund" -> "Refund", "source" -> "Source", "sourced_transfers" -> "SourcedTransfers", "status" -> "Status", "transfer" -> "Transfer", 
	"transfer_cancel" -> "TransferCancel", "transfer_failure" -> "TransferFailure", "type" -> "Type", Null -> Missing["NotAvailable"]}

$StripeBalanceTransactionFeeCamel = {
	"amount" -> "Amount", "application" -> "Application", "application_fee" -> "ApplicationFee", "description" -> "Description",
	Null -> Missing["NotAvailable"], "stripe_fee" -> "StripeFee", "tax" -> "Tax", "type" -> "Type"}

End[];
 
EndPackage[];