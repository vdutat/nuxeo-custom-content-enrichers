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
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Spliterator;
import java.util.Spliterators;
import java.util.stream.Collectors;
import java.util.stream.StreamSupport;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.nuxeo.ecm.core.api.CoreSession;
import org.nuxeo.ecm.core.api.DocumentModel;
import org.nuxeo.ecm.core.api.IterableQueryResult;
import org.nuxeo.ecm.core.api.LifeCycleConstants;
import org.nuxeo.ecm.core.api.impl.DocumentModelListImpl;
import org.nuxeo.ecm.core.io.marshallers.json.enrichers.AbstractJsonEnricher;
import org.nuxeo.ecm.core.io.registry.reflect.Setup;
import org.nuxeo.ecm.core.query.sql.NXQL;
import org.nuxeo.ecm.core.query.sql.model.Operator;
import org.nuxeo.ecm.core.schema.FacetNames;

import com.fasterxml.jackson.core.JsonGenerator;

/**
 * Retrieves grand children documents of a document.
 *
 * See https://jira.nuxeo.com/browse/SUPNXP-18329
 * @author vdutat
 *
 */
@Setup(mode = SINGLETON, priority = REFERENCE)
public class GrandChildrenContentEnricher extends AbstractJsonEnricher<DocumentModel> {

    protected static final Log log = LogFactory.getLog(GrandChildrenContentEnricher.class);

    public static final String NAME = "grandchildrenDocuments";

    public GrandChildrenContentEnricher() {
        super(NAME);
    }

    @Override
    public void write(JsonGenerator jg, DocumentModel doc) throws IOException {
        // collect document IDs of children documents
        StringBuilder query = new StringBuilder("SELECT " + NXQL.ECM_UUID + " FROM Document WHERE ");
        query.append(NXQL.ECM_PARENTID).append(Operator.EQ.toString()).append(NXQL.escapeString(doc.getId()))
        .append(" " + Operator.AND.toString() + " ").append(NXQL.ECM_MIXINTYPE).append(Operator.EQ.toString()).append(NXQL.escapeString(FacetNames.FOLDERISH))
        .append(" " + Operator.AND.toString() + " ").append(NXQL.ECM_LIFECYCLESTATE).append(Operator.NOTEQ.toString()).append(NXQL.escapeString(LifeCycleConstants.DELETED_STATE))
        .append(" " + Operator.AND.toString() + " ").append(NXQL.ECM_ISCHECKEDIN).append(Operator.EQ.toString() + "0");
        if (log.isDebugEnabled()) {
            log.debug("<write> query: " + query.toString());
        }
        List<String> uuids = getUuids(doc.getCoreSession(), query.toString());
        // collect grand children documents
        // DOES NOT WORK
//        List<DocumentModel> docs = uuids.stream().flatMap(id -> getChildren(doc.getCoreSession(), id).stream()).collect(Collectors.toList());
        List<DocumentModel> docs = new DocumentModelListImpl();
        for (String id : uuids) {
            docs.addAll(getChildren(doc.getCoreSession(), id));
        }
        if (log.isDebugEnabled()) {
            log.debug("<write> docs: " + docs);
        }
        // write JSON
        jg.writeFieldName(NAME);
        writeEntity(docs, jg);

    }

    protected List<String> getUuids(CoreSession session, String query) {
        List<String> ids = new ArrayList<String>();
        IterableQueryResult it = null;
        try {
            it = session.queryAndFetch(query, NXQL.NXQL);
            Spliterator<Map<String, Serializable>> spliterator = Spliterators.spliteratorUnknownSize(it.iterator(), Spliterator.NONNULL);
            ids = StreamSupport.stream(spliterator, false).map(map -> (String) map.get(NXQL.ECM_UUID)).collect(Collectors.toList());
        } finally {
            if (it != null) {
                it.close();
            }
        }
        return ids;
    }

    protected List<DocumentModel> getChildren(CoreSession session, String parentId) {
        StringBuilder query = new StringBuilder("SELECT * FROM Document WHERE ");
        query.append(NXQL.ECM_PARENTID).append(Operator.EQ.toString()).append(NXQL.escapeString(parentId))
        .append(" " + Operator.AND.toString() + " ").append(NXQL.ECM_LIFECYCLESTATE).append(Operator.NOTEQ.toString()).append(NXQL.escapeString(LifeCycleConstants.DELETED_STATE))
        .append(" " + Operator.AND.toString() + " ").append(NXQL.ECM_ISCHECKEDIN).append(Operator.EQ.toString() + "0");
        List<DocumentModel> docs = session.query(query.toString());
        if (log.isDebugEnabled()) {
            log.debug("<getChildren> query: " + query.toString() + ", " + docs);
        }
        return docs;
    }

}
