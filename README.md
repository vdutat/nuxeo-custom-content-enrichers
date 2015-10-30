# nuxeo-custom-content-enrichers

This module provides a `content enricher` named `allowedDocumentTypes` supplying the list of document types that can be created beneath the requested document.
See Nuxeo documentation [Content Enricher](https://doc.nuxeo.com/display/NXDOC60/Content+Enricher) to know how to use `content enricher`.

 
## Building

        mvn clean install

## Using

All you have to do is:

 - copy the bundle in `nxserver/plugins` or `nxserver/bundles`
 - restart the server
 