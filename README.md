# media-goggler
A media server written in Haskell and Cycle.js

To run this locally use `docker-compose`.
Mount the folder with your libraries to `/media` inside the container (adjust the path in the `docker-compose.yaml`).

## Why not Plex?

I have several issues with Plex:
- Not open source
- Only one user in the free version
- Can only host video and audio files
- DLNA somewhat broken (at least works not for me)

## Why not Emby?

There are also some issues with Emby:
- Annoying "Please get Premium" messages when playing video
- Ugly UI
- No endless scrolling when viewing an overview (and pagination does not jump back to the top)
- Can only host video and audio files
- Users are not local on the server
- Transcoding does not work correctly (at least for me)

## What about Kodi?

Kodi is a fantastic client that runs on many devices including the Raspberry PI. But it is not a media server. I want one central master server I can host on my NAS and have many clients that can use the metadata from that server. I [plan](https://github.com/jvanbruegge/media-goggler/issues/8) to add a Kodi plugin that can access the API of the server

## Why Haskell?

Because it is an amazing language! The type safety and the compiler help a lot when writing code. Additionally the library eco system has many amazing and performat abstractions for common problems. For example, implementing HTTP range requests would have been a lot harder without `servant` and `conduit`.
