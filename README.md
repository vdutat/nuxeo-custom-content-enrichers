# nuxeo-custom-content-enrichers

This module provides a `content enricher` named `allowedDocumentTypes` supplying the list of document types that can be created beneath the requested document.
See Nuxeo documentation [Content Enricher](https://doc.nuxeo.com/display/NXDOC60/Content+Enricher) to know how to use `content enricher`.

## Result JSON sample

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

## Building

        mvn clean install

## Using

All you have to do is:

 - copy the bundle in `nxserver/plugins` or `nxserver/bundles`
 - restart the server
 