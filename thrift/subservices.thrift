
namespace cpp multiservice
namespace java pl.waw.ipipan.multiservice.thrift.services
namespace py multiservice.subservices

include "types.thrift"

service InputService {
    types.TText preprocess(1: string text, 2: map<string, string> options) 
                throws (1: types.MultiserviceException ex)
}

service AnnotatingService {
    types.TText annotate(1: types.TText text, 2: map<string, string> options)
                throws (1: types.MultiserviceException ex)
}

service OutputService {
    string postprocess(1: types.TText text, 2: map<string, string> options)
                throws (1: types.MultiserviceException ex)
}
