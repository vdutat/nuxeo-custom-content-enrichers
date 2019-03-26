package org.nuxeo.enrichers;

import static org.nuxeo.ecm.core.io.registry.reflect.Instantiations.SINGLETON;
import static org.nuxeo.ecm.core.io.registry.reflect.Priorities.REFERENCE;

import java.io.IOException;

import org.nuxeo.ecm.core.api.DocumentModel;
import org.nuxeo.ecm.core.api.DocumentModelList;
import org.nuxeo.ecm.core.io.marshallers.json.enrichers.AbstractJsonEnricher;
import org.nuxeo.ecm.core.io.registry.reflect.Setup;

import com.fasterxml.jackson.core.JsonGenerator;

@Setup(mode = SINGLETON, priority = REFERENCE)
public class PublishDocumentEnricher extends AbstractJsonEnricher<DocumentModel> { 
 
    public static final String NAME = "publishedDocuments";
    
    public PublishDocumentEnricher() {
        super(NAME);
    }
 
    @Override
    public void write(JsonGenerator jg, DocumentModel doc) throws IOException {
      DocumentModelList proxies = doc.getCoreSession().getProxies(doc.getRef(), null);
      jg.writeFieldName(NAME);
      jg.writeStartArray();
      for(DocumentModel proxy : proxies) {
    	  jg.writeStartObject();
    	  jg.writeStringField("id", proxy.getId());
    	  jg.writeStringField("name", proxy.getName());
    	  jg.writeStringField("path", proxy.getPathAsString());
    	  jg.writeEndObject();
      }
      jg.writeEndArray();
      jg.flush();
    }
}
