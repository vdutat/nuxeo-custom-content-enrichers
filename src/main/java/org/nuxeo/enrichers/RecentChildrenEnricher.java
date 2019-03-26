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
 *
 */
package org.nuxeo.enrichers;

import static org.nuxeo.ecm.core.io.registry.reflect.Instantiations.SINGLETON;
import static org.nuxeo.ecm.core.io.registry.reflect.Priorities.REFERENCE;

import java.io.IOException;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.nuxeo.ecm.automation.core.scripting.DateWrapper;
import org.nuxeo.ecm.core.api.CoreSession;
import org.nuxeo.ecm.core.api.DocumentModel;
import org.nuxeo.ecm.core.api.LifeCycleConstants;
import org.nuxeo.ecm.core.api.PathRef;
import org.nuxeo.ecm.core.io.marshallers.json.enrichers.AbstractJsonEnricher;
import org.nuxeo.ecm.core.io.registry.context.RenderingContext.SessionWrapper;
import org.nuxeo.ecm.core.io.registry.reflect.Setup;
import org.nuxeo.ecm.core.query.sql.NXQL;
import org.nuxeo.ecm.core.query.sql.model.Operator;

import com.fasterxml.jackson.core.JsonGenerator;

/**
 * Enrich {@link nuxeo.ecm.core.api.DocumentModel} Json.
 * <p>
 * Format is:
 *
 * <pre>
 * {@code
 * {
 *   ...
 *   "contextParameters": {
 *     "recent_children": { ... }
 *   }
 * }
 * </pre>
 * </p>
 * See https://jira.nuxeo.com/browse/SUPNXP-18599
 * @author vdutat
 */
@Setup(mode = SINGLETON, priority = REFERENCE)
public class RecentChildrenEnricher extends AbstractJsonEnricher<DocumentModel> {

    protected static final Log log = LogFactory.getLog(RecentChildrenEnricher.class);

    public static final String NAME = "recent_children";

    public RecentChildrenEnricher() {
        super(NAME);
    }

    @Override
    public void write(JsonGenerator jg, DocumentModel doc) throws IOException {
        List<DocumentModel> children = null;

        String parentPath = ctx.getParameter("parentPath");
        if (log.isDebugEnabled()) {
            if (parentPath != null) {
                log.debug("<getChildren> parentPath=" + parentPath);
            }
        }
        String parentId = doc.getId();
        try (SessionWrapper wrapper = ctx.getSession(doc)) {
            if (parentPath != null) {
                DocumentModel parentDoc = wrapper.getSession().getDocument(new PathRef(parentPath));
                parentId = parentDoc.getId();
            }
            children = getChildren(wrapper.getSession(), parentId);
        }
        jg.writeFieldName(NAME);
        writeEntity(children, jg);
    }

    protected List<DocumentModel> getChildren(CoreSession session, String parentId) {
        StringBuilder query = new StringBuilder("SELECT * FROM Document WHERE ");
        query.append(NXQL.ECM_PARENTID).append(Operator.EQ.toString()).append(NXQL.escapeString(parentId))
        .append(" " + Operator.AND.toString() + " ").append("dc:modified").append(Operator.GTEQ.toString()).append(new DateWrapper().days(-30).toString())
        .append(" " + Operator.AND.toString() + " ").append(NXQL.ECM_LIFECYCLESTATE).append(Operator.NOTEQ.toString()).append(NXQL.escapeString(LifeCycleConstants.DELETED_STATE))
        .append(" " + Operator.AND.toString() + " ").append(NXQL.ECM_ISCHECKEDIN).append(Operator.EQ.toString() + "0");
        List<DocumentModel> docs = session.query(query.toString());
        if (log.isDebugEnabled()) {
            log.debug("<getChildren> query: " + query.toString() + ", " + docs);
        }
        return docs;
    }
}
