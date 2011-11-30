module Troyd.Protbuf (protoInfo, fileDescriptorProto) where
import Prelude ((+))
import qualified Prelude as P'
import qualified Text.ProtocolBuffers.Header as P'
import Text.DescriptorProtos.FileDescriptorProto (FileDescriptorProto)
import Text.ProtocolBuffers.Reflections (ProtoInfo)
import qualified Text.ProtocolBuffers.WireMessage as P' (wireGet,getFromBS)
 
protoInfo :: ProtoInfo
protoInfo
 = P'.read
    "ProtoInfo {protoMod = ProtoName {protobufName = FIName \".troyd.protbuf\", haskellPrefix = [], parentModule = [MName \"Troyd\"], baseName = MName \"Protbuf\"}, protoFilePath = [\"Troyd\",\"Protbuf.hs\"], protoSource = \"troyd.proto\", extensionKeys = fromList [], messages = [DescriptorInfo {descName = ProtoName {protobufName = FIName \".troyd.protbuf.ReqOrderAdd\", haskellPrefix = [], parentModule = [MName \"Troyd\",MName \"Protbuf\"], baseName = MName \"ReqOrderAdd\"}, descFilePath = [\"Troyd\",\"Protbuf\",\"ReqOrderAdd.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".troyd.protbuf.ReqOrderAdd.instrument\", haskellPrefix' = [], parentModule' = [MName \"Troyd\",MName \"Protbuf\",MName \"ReqOrderAdd\"], baseName' = FName \"instrument\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False}], enums = [], knownKeyMap = fromList []}"
 
fileDescriptorProto :: FileDescriptorProto
fileDescriptorProto
 = P'.getFromBS (P'.wireGet 11)
    (P'.pack "?\n\vtroyd.proto\DC2\rtroyd.protbuf\"!\n\vReqOrderAdd\DC2\DC2\n\ninstrument\CAN\SOH \STX(\t")