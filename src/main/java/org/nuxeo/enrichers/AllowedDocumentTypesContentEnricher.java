package org.nuxeo.enrichers;

import static org.nuxeo.ecm.core.io.registry.reflect.Instantiations.SINGLETON;
import static org.nuxeo.ecm.core.io.registry.reflect.Priorities.REFERENCE;

import java.io.IOException;
import java.util.Collection;

import org.nuxeo.ecm.core.api.DocumentModel;
import org.nuxeo.ecm.core.io.marshallers.json.enrichers.AbstractJsonEnricher;
import org.nuxeo.ecm.core.io.registry.reflect.Setup;
import org.nuxeo.ecm.platform.types.Type;
import org.nuxeo.ecm.platform.types.TypeManager;
import org.nuxeo.runtime.api.Framework;

import com.fasterxml.jackson.core.JsonGenerator;

//The class will be instanciated as a singleton
//Priority defines which marshaller will be used in case of conflict. Priority is an integer.
//The higher the number, the more priority you get: 10 > 1 for instance.
@Setup(mode = SINGLETON, priority = REFERENCE)
public class AllowedDocumentTypesContentEnricher extends AbstractJsonEnricher<DocumentModel> {
    
    // The enricher will be called using X-NXenrichers.document: allowedDocumentTypes
    public static final String ID= "allowedDocumentTypes";

    public AllowedDocumentTypesContentEnricher() {
        super(ID);
    }

    @Override
    public void write(JsonGenerator jg, DocumentModel doc) throws IOException {
        TypeManager typeManager = Framework.getService(TypeManager.class);
        Collection<Type> subTypes = typeManager.getAllowedSubTypes(doc.getType(), doc);
        jg.writeFieldName(ID);
        jg.writeStartArray();
        for (Type subType : subTypes) {
            jg.writeStartObject();
            jg.writeStringField("id", subType.getId());
            jg.writeStringField("label", subType.getLabel());
            jg.writeStringField("category", subType.getCategory());
            jg.writeStringField("description", subType.getDescription());
            jg.writeStringField("icon", subType.getIcon());
            jg.writeStringField("bigIcon", subType.getBigIcon());
            jg.writeEndObject();
        }
        jg.writeEndArray();
        jg.flush();
    }

}
