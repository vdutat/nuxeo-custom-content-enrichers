package org.nuxeo.enrichers;

import java.io.IOException;
import java.util.Collection;

import org.codehaus.jackson.JsonGenerator;
import org.nuxeo.ecm.automation.io.services.enricher.AbstractContentEnricher;
import org.nuxeo.ecm.automation.io.services.enricher.RestEvaluationContext;
import org.nuxeo.ecm.core.api.ClientException;
import org.nuxeo.ecm.core.api.DocumentModel;
import org.nuxeo.ecm.platform.types.Type;
import org.nuxeo.ecm.platform.types.TypeManager;
import org.nuxeo.runtime.api.Framework;

public class AllowedDocumentTypesContentEnricher extends AbstractContentEnricher {

    public static final String ID= "allowedDocumentTypes";

    @Override
    public void enrich(JsonGenerator jg, RestEvaluationContext ec) throws ClientException, IOException {
        DocumentModel doc = ec.getDocumentModel();
        TypeManager typeManager = Framework.getService(TypeManager.class);
        Collection<Type> subTypes = typeManager.getAllowedSubTypes(doc.getType(), doc);
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
