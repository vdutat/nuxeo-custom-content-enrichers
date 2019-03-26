/*
 * (C) Copyright 2016 Nuxeo SA (http://nuxeo.com/) and others.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * Contributors:
 *     vdutat
 */
package org.nuxeo.enrichers;

import static org.nuxeo.ecm.core.io.registry.reflect.Instantiations.SINGLETON;
import static org.nuxeo.ecm.core.io.registry.reflect.Priorities.REFERENCE;

import java.io.IOException;
import java.util.Calendar;
import java.util.List;

import org.apache.commons.collections.CollectionUtils;
import org.codehaus.jackson.JsonGenerationException;
import org.nuxeo.ecm.core.api.DocumentModel;
import org.nuxeo.ecm.core.io.marshallers.json.enrichers.AbstractJsonEnricher;
import org.nuxeo.ecm.core.io.registry.reflect.Setup;
import org.nuxeo.ecm.core.schema.utils.DateParser;
import org.nuxeo.ecm.platform.comment.api.CommentableDocument;

import com.fasterxml.jackson.core.JsonGenerator;

@Setup(mode = SINGLETON, priority = REFERENCE)
public class CommentsContentEnricher extends AbstractJsonEnricher<DocumentModel> {

    public static final String NAME = "comments";

    public CommentsContentEnricher() {
        super(NAME);
    }

    @Override
    public void write(JsonGenerator jg, DocumentModel doc) throws IOException {
        jg.writeFieldName(NAME);
        CommentableDocument adapter = doc.getAdapter(CommentableDocument.class);
        printComments(jg, adapter, null);
        jg.flush();
    }

    private void printComments(JsonGenerator jg, CommentableDocument adapter, DocumentModel commentDoc)
            throws IOException, JsonGenerationException {
        List<DocumentModel> comments = commentDoc == null ? adapter.getComments() : adapter.getComments(commentDoc);
        if (!CollectionUtils.isEmpty(comments)) {
            if (commentDoc != null) {
                jg.writeFieldName("replies");
            }
            jg.writeStartArray();
            for(DocumentModel comment : comments) {
                jg.writeStartObject();
                jg.writeStringField("id", comment.getId());
                jg.writeStringField("text", (String) comment.getPropertyValue("comment:text"));
                jg.writeStringField("author", (String) comment.getPropertyValue("comment:author"));
                jg.writeStringField("creationDate", DateParser.formatW3CDateTime(((Calendar) comment.getPropertyValue("comment:creationDate")).getTime()));
                printComments(jg, adapter, comment);
                jg.writeEndObject();
            }
            jg.writeEndArray();
        }
    }

}
