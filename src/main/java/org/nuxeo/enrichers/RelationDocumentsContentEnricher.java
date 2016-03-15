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

import java.io.Closeable;
import java.io.IOException;
import java.security.Principal;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.collections.Predicate;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.codehaus.jackson.JsonGenerator;
import org.nuxeo.ecm.core.api.DocumentModel;
import org.nuxeo.ecm.core.api.DocumentModelList;
import org.nuxeo.ecm.core.api.IdRef;
import org.nuxeo.ecm.core.api.UnrestrictedSessionRunner;
import org.nuxeo.ecm.core.api.security.SecurityConstants;
import org.nuxeo.ecm.core.io.marshallers.json.enrichers.AbstractJsonEnricher;
import org.nuxeo.ecm.core.io.registry.context.MaxDepthReachedException;
import org.nuxeo.ecm.core.io.registry.context.RenderingContext.SessionWrapper;
import org.nuxeo.ecm.core.io.registry.reflect.Setup;
import org.nuxeo.ecm.core.query.sql.NXQL;
import org.nuxeo.ecm.core.query.sql.model.Operator;

@Setup(mode = SINGLETON, priority = REFERENCE)
public class RelationDocumentsContentEnricher extends AbstractJsonEnricher<DocumentModel> {

    private static final Log LOG = LogFactory.getLog(RelationDocumentsContentEnricher.class);

    public static final String NAME = "relationDocuments";

    public RelationDocumentsContentEnricher() {
        super(NAME);
    }

    @Override
    public void write(JsonGenerator jg, DocumentModel document) throws IOException {
        Principal principal = ctx.getSession(document).getSession().getPrincipal();
        try (SessionWrapper wrapper = ctx.getSession(document)) {
            String id = document.getId();
            new UnrestrictedSessionRunner(wrapper.getSession()) {
                @Override
                public void run() {
                    // control the graph depth
                    try (Closeable resource = ctx.wrap().controlDepth().open()) {
                        StringBuilder query = new StringBuilder("SELECT * FROM ");
                        query.append("DefaultRelation").append(" WHERE ")
                        .append("relation:source").append("=").append(NXQL.escapeString(id))
                        .append(Operator.OR.toString()).append(" " + "relation:target" + " ").append("=").append(NXQL.escapeString(id));
                        DocumentModelList relations = session.query(query.toString());
                        CollectionUtils.filter(relations, new Predicate() {
                            @Override
                            public boolean evaluate(Object object) {
                                DocumentModel doc = (DocumentModel) object;
                                if (!session.hasPermission(principal, new IdRef((String) doc.getPropertyValue("relation:source")), SecurityConstants.READ)
                                        || !session.hasPermission(principal, new IdRef((String) doc.getPropertyValue("relation:target")), SecurityConstants.READ)) {
                                    return false;
                                }
                                return true;
                            }});
                        jg.writeFieldName(NAME);
                        // delegate the marshalling to Nuxeo Platform
                        writeEntity(relations, jg);
                    } catch (MaxDepthReachedException e) {
                        // do not apply enricher
                    } catch (IOException e) {
                        LOG.error(e, e);
                    }

                }
            }.runUnrestricted();
        }

    }

}
