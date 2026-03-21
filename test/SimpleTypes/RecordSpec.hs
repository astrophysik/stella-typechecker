module SimpleTypes.RecordSpec (spec) where

import Test.Hspec
import TestUtil
import TypeCheck.TypeCheck (unexpectedFieldAccess, notARecord, unexpectedRecord, missingRecordFields, unexpectedRecordFields)

spec :: Spec
spec = describe "record type tests" $ do
  it "record type" $ do
    shouldTypecheckFile "test/Programs/SimpleTypes/Record/record.stella"
  it "ill typed field access" $ do
    shouldFailFileWith "test/Programs/SimpleTypes/Record/ill-typed-field-access.stella" unexpectedFieldAccess
  it "ill typed not a record" $ do
    shouldFailFileWith "test/Programs/SimpleTypes/Record/ill-typed-not-record.stella" notARecord
  it "ill typed unexpected record" $ do
    shouldFailFileWith "test/Programs/SimpleTypes/Record/ill-typed-unexpected-record.stella" unexpectedRecord
  it "ill typed unexpected record call" $ do
    shouldFailFileWith "test/Programs/SimpleTypes/Record/ill-typed-unexpected-record-call.stella" unexpectedRecord
  it "ill typed missing record fields" $ do
    shouldFailFileWith "test/Programs/SimpleTypes/Record/ill-typed-missing-fields.stella" missingRecordFields
  it "ill typed missing record fields in lambda" $ do
    shouldFailFileWith "test/Programs/SimpleTypes/Record/ill-typed-missing-fields-lambda.stella" missingRecordFields
  it "ill typed unexpected record fields" $ do
    shouldFailFileWith "test/Programs/SimpleTypes/Record/ill-typed-unexpected-fields.stella" unexpectedRecordFields
