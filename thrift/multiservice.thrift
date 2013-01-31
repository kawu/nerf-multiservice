
namespace cpp multiservice
namespace java pl.waw.ipipan.multiservice.thrift
namespace py multiservice.facade

include "types.thrift"

enum RequestStatus {
   PENDING,
   IN_PROGRESS,
   DONE,
   FAILED,
   DUMPED
}

enum OptionType {
   STRING,
   INT,
   FLOAT,
   BOOLEAN
}

enum ServiceType {
   INPUT,
   ANNOTATING,
   OUTPUT
}

struct RequestPart {
        1: string serviceName,
        2: map<string, string> options = []
}

struct StringRequest {
        1: string text,
        2: RequestPart inputSettings,
        4: list<RequestPart> processingChain,
        5: RequestPart outputSettings
}

struct ObjectRequest {
        1: types.TText text,
        2: list<RequestPart> processingChain
}

struct DaemonInfo {
	1: string serviceName,
	2: string host,
	3: i32 port,
	4: bool active
}

struct OptionInfo {
	1: string name,
	2: OptionType type,
	3: string defaultValue,
	4: bool mandatory = false
}

struct ServiceInfo {
	1: string name,
	2: ServiceType serviceType,
	3: list<types.TAnnotationLayer> requiredLayers = [],
	4: list<types.TAnnotationLayer> providedLayers,
	5: list<OptionInfo> availableOptions = [],
	
}

exception RequestManagerException {
    1: string message
}

service Multiservice {
        
        string putStringRequest(1: StringRequest request),
        
        string putObjectRequest(1: ObjectRequest request),
        
        RequestStatus getRequestStatus(1: string requestToken),
        
        string getResultString(1: string requestToken),
        
        types.TText getResultObject(1: string requestToken),
        
        types.MultiserviceException getException(1: string requestToken),
        
        list<ServiceInfo> getServices(1: ServiceType serviceType),
        
        list<DaemonInfo> getAllDaemons(),
        
        list<DaemonInfo> getDaemons4Service(1: string serviceName),
        
        list<types.TAnnotationLayer> getLayersProvidedByService(1: string serviceName),
        
        list<types.TAnnotationLayer> getLayersRequiredByService(1: string serviceName),
}
