# nuxeo-custom-content-enrichers

This module provides custom content enrichers.
See Nuxeo documentation [Content Enricher](https://doc.nuxeo.com/display/NXDOC60/Content+Enricher) to know how to use `content enricher`.

## allowedDocumentTypes

This `content enricher` supplies the list of document types that can be created beneath the requested document.

### Result JSON sample

Here is an extract of a JSON payload return by a `GET` request on a `Workspace` document:

```json

  "contextParameters": {
    "allowedDocumentTypes": [
      {
        "bigIcon": "/icons/ordered_folder_100.png",
        "icon": "/icons/ordered_folder.png",
        "description": "OrderedFolder.description",
        "category": "Collaborative",
        "label": "OrderedFolder",
        "id": "OrderedFolder"
      },
      {
        "bigIcon": "/img/folder_orange_100.png",
        "icon": "/img/folder_orange.png",
        "description": "",
        "category": "Collaborative",
        "label": "Folder",
        "id": "Folder"
      },
      {
        "bigIcon": "/icons/forum_100.png",
        "icon": "/icons/forum.gif",
        "description": "Forum.description",
        "category": "Collaborative",
        "label": "Forum",
        "id": "Forum"
      },
      {
        "bigIcon": "/icons/image_100.png",
        "icon": "/icons/image.gif",
        "description": "Picture.description",
        "category": "SimpleDocument",
        "label": "Picture",
        "id": "Picture"
      },
      {
        "bigIcon": "/img/easyshare_100.png",
        "icon": "/img/easyshare.png",
        "description": "Container for holding shared files and accessible anonymously by URL",
        "category": "Collaborative",
        "label": "Easy Share Folder",
        "id": "EasyShareFolder"
      },
      {
        "bigIcon": "/icons/video_big.png",
        "icon": "/icons/video.png",
        "description": "Video.description",
        "category": "SimpleDocument",
        "label": "Video",
        "id": "Video"
      },
      {
        "bigIcon": "/icons/note_100.png",
        "icon": "/icons/note.gif",
        "description": "Note.description",
        "category": "SimpleDocument",
        "label": "Note",
        "id": "Note"
      },
      {
        "bigIcon": "/icons/collection_100.png",
        "icon": "/icons/collection.png",
        "description": "",
        "category": "Collaborative",
        "label": "Collection",
        "id": "Collection"
      },
      {
        "bigIcon": "/icons/audio_100.png",
        "icon": "/icons/audio.png",
        "description": "Audio.description",
        "category": "SimpleDocument",
        "label": "Audio",
        "id": "Audio"
      },
      {
        "bigIcon": "/img/file_100.png",
        "icon": "/img/file.png",
        "description": "",
        "category": "SimpleDocument",
        "label": "File",
        "id": "File"
      },
      {
        "bigIcon": "/img/folder_green_100.png",
        "icon": "/img/folder_green.png",
        "description": "",
        "category": "Collaborative",
        "label": "Workspace",
        "id": "Workspace"
      }
    ]
  },


```

## comments

This `content enricher` supplies the comments and their replies made on a document.

### Result JSON sample

Here is an extract of a JSON payload return by a `GET` request on a document:

```json

  contextParameters: 
   { comments: 
      [ { text: 'Comment 1 from Admin',
          author: 'Administrator',
          creationDate: '2016-02-01T23:00:49.13Z',
          replies: 
           [ { text: 'Reply 1 from vdu1',
               author: 'vdu1',
               creationDate: '2016-02-01T23:07:44.20Z',
               replies: 
                [ { text: 'Reply 2 from Admin',
                    author: 'Administrator',
                    creationDate: '2016-02-02T00:51:15.83Z' } ] } ] },
        { text: 'Comment 2 from Admin',
          author: 'Administrator',
          creationDate: '2016-02-02T00:51:31.44Z' } ] } 
```

## publishedDocuments

This `content enricher` supplies the list of published proxies for the document.

### Result JSON sample

Here is an extract of a JSON payload return by a `GET` request on a document:

```json

  contextParameters: 
   { publishedDocuments: 
      [ { id: '57800830-ebae-4cb8-a032-bac6e7755755',
          name: 'File 2',
          path: '/default-domain/sections/Section 1/File 2' } ] }
```

## tags

This `content enricher` supplies the tags assigned to the document.

### Result JSON sample

Here is an extract of a JSON payload return by a `GET` request on a document:

```json
  contextParameters: 
   { tags: 
      [ { label: 'tag1', weight: '0' },
        { label: 'tag2', weight: '0' } ] }
```

## Building

        mvn clean install

## Using

All you have to do is:

 - copy the bundle in `nxserver/plugins` or `nxserver/bundles`
 - restart the server
 
