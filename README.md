## About Studio
Studio is an image gallery hosting software built on [Radiance](https://shirakumo.github.io/radiance-homepage). Unlike other hosting sites it explicitly omits any kind of social networking features and instead presents an artist-centred view, allowing artists to host their own portfolio.

## Features

* Artist-focused galleries
* Per-month view of uploads
* Multiple images per uploads
* Additional viewing modes optimised for comics
* Public, unlisted, and private uploads
* Upload tagging and browsing by tag
* Tumblr import
* Per-gallery RSS feeds
* Mobile-ready layout
* Complete REST API
* No requirement for JavaScript

## Live Instance
A live instance of studio is available on [TyNET](https://studio.tymoon.eu/).

## Studio Setup
### Configuration Variables

    :allowed-content-types  --- What content types are allowed to be uploaded
                                Defaults to: '("image/png" "image/jpeg" "image/gif" "image/svg+xml")
    :per-page :uploads      --- How many uploads to list per page on a gallery
                                Defaults to: 40
    :per-page :galleries    --- How many galleries to list per page on the frontpage
                                Defaults to: 10
    :frontpage-uploads      --- How many uploads to show per user on the frontpage
                                Defaults to: 4
    :thumbnail-size         --- The size of a thumbnail in pixels
                                Defaults to: 400
    :permissions :default   --- The list of default permissions
    :tumblr :api-key        --- The tumblr oAuth application key
    :tumblr :api-secret     --- The tumblr oAuth application secret

### Permissions

    studio                     --- All studio permissions
    studio.gallery             --- Permissions related to user galleries
    studio.gallery.create      --- Whether a user can create a gallery for themselves
    studio.gallery.edit        --- Whether a user can edit any gallery
    studio.gallery.edit.own    --- Whether the user can edit their own gallery
    studio.gallery.delete      --- Whether a user can delete any gallery
    studio.gallery.delete.own  --- Whether the user can edit their own gallery
    studio.upload.import       --- Whether a user can use the import feature
    studio.upload.create       --- Whether a user can upload new images
    studio.upload.edit         --- Whether a user can edit any image
    studio.upload.edit.own     --- Whether the user can edit their own images
    studio.upload.delete       --- Whether a user can delete any image
    studio.upload.delete.own   --- Whether the user can delete their own images
    
By default all the leaf permissions are granted to users.
