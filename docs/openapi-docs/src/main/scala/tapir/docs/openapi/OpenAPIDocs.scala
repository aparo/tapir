package tapir.docs.openapi
import tapir.Endpoint
import tapir.openapi.OpenAPI

trait OpenAPIDocs {
  implicit class RichOpenAPIEndpoint[I, E, O](e: Endpoint[I, E, O]) {
    def toOpenAPI(title: String, version: String): OpenAPI =
      EndpointToOpenAPIDocs.toOpenAPI(title, version, Seq(e))
  }

  implicit class RichOpenAPIEndpoints(es: Iterable[Endpoint[_, _, _]]) {
    def toOpenAPI(title: String, version: String): OpenAPI = EndpointToOpenAPIDocs.toOpenAPI(title, version, es)
  }
}
