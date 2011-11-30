module Troyd.Protbuf.ReqOrderAdd (ReqOrderAdd(..)) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
 
data ReqOrderAdd = ReqOrderAdd{instrument :: P'.Utf8}
                 deriving (P'.Show, P'.Eq, P'.Ord, P'.Typeable)
 
instance P'.Mergeable ReqOrderAdd where
  mergeEmpty = ReqOrderAdd P'.mergeEmpty
  mergeAppend (ReqOrderAdd x'1) (ReqOrderAdd y'1) = ReqOrderAdd (P'.mergeAppend x'1 y'1)
 
instance P'.Default ReqOrderAdd where
  defaultValue = ReqOrderAdd P'.defaultValue
 
instance P'.Wire ReqOrderAdd where
  wireSize ft' self'@(ReqOrderAdd x'1)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeReq 1 9 x'1)
  wirePut ft' self'@(ReqOrderAdd x'1)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutReq 10 9 x'1
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> P'.fmap (\ new'Field -> old'Self{instrument = new'Field}) (P'.wireGet 9)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> ReqOrderAdd) ReqOrderAdd where
  getVal m' f' = f' m'
 
instance P'.GPB ReqOrderAdd
 
instance P'.ReflectDescriptor ReqOrderAdd where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [10]) (P'.fromDistinctAscList [10])
  reflectDescriptorInfo _
   = P'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".troyd.protbuf.ReqOrderAdd\", haskellPrefix = [], parentModule = [MName \"Troyd\",MName \"Protbuf\"], baseName = MName \"ReqOrderAdd\"}, descFilePath = [\"Troyd\",\"Protbuf\",\"ReqOrderAdd.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".troyd.protbuf.ReqOrderAdd.instrument\", haskellPrefix' = [], parentModule' = [MName \"Troyd\",MName \"Protbuf\",MName \"ReqOrderAdd\"], baseName' = FName \"instrument\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False}"