/*------------------------------------------------------------------------
 *  Copyright 2007-2010 (c) Jeff Brown <spadix@users.sourceforge.net>
 *
 *  This file is part of the ZBar Bar Code Reader.
 *
 *  The ZBar Bar Code Reader is free software; you can redistribute it
 *  and/or modify it under the terms of the GNU Lesser Public License as
 *  published by the Free Software Foundation; either version 2.1 of
 *  the License, or (at your option) any later version.
 *
 *  The ZBar Bar Code Reader is distributed in the hope that it will be
 *  useful, but WITHOUT ANY WARRANTY; without even the implied warranty
 *  of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser Public License
 *  along with the ZBar Bar Code Reader; if not, write to the Free
 *  Software Foundation, Inc., 51 Franklin St, Fifth Floor,
 *  Boston, MA  02110-1301  USA
 *
 *  http://sourceforge.net/projects/zbar
 *------------------------------------------------------------------------*/

#include "video.h"
#include "image.h"


#ifdef HAVE_LIBJPEG
extern struct jpeg_decompress_struct *_zbar_jpeg_decomp_create(void);
extern void _zbar_jpeg_decomp_destroy(struct jpeg_decompress_struct *cinfo);
#endif

// from ezdib
static const char font_map_medium [] =
{
	// Default glyph
	'.', 2, 10,	0x00, 0x3c, 0x00,

	// Tab width
	'\t', 10, 0,

	// Space
	' ', 2, 0,

	'!', 1, 10,	0xf6, 0x00,
	'(', 3, 10,	0x2a, 0x48, 0x88, 0x00,
	')', 3, 10,	0x88, 0x92, 0xa0, 0x00,
	',', 2, 10,	0x00, 0x16, 0x00,
	'-', 3, 10,	0x00, 0x70, 0x00, 0x00,
	'/', 3, 10,	0x25, 0x25, 0x20, 0x00,
	'@', 6, 10,	0x7a, 0x19, 0x6b, 0x9a, 0x07, 0x80, 0x00, 0x00,
	'$', 5, 10,	0x23, 0xab, 0x47, 0x16, 0xae, 0x20, 0x00,
	'#', 6, 10,	0x49, 0x2f, 0xd2, 0xfd, 0x24, 0x80, 0x00, 0x00,
	'%', 7, 10,	0x43, 0x49, 0x20, 0x82, 0x49, 0x61, 0x00, 0x00, 0x00,
	':', 2, 10,	0x3c, 0xf0, 0x00,
	'^', 3, 10,	0x54, 0x00, 0x00, 0x00,
	'~', 5, 10,	0x00, 0x11, 0x51, 0x00, 0x00, 0x00, 0x00,

	'0', 5, 10,	0x74, 0x73, 0x59, 0xc5, 0xc0, 0x00, 0x00,
	'1', 3, 10,	0xc9, 0x24, 0xb8, 0x00,
	'2', 5, 10,	0x74, 0x42, 0xe8, 0x43, 0xe0, 0x00, 0x00,
	'3', 5, 10,	0x74, 0x42, 0xe0, 0xc5, 0xc0, 0x00, 0x00,
	'4', 5, 10,	0x11, 0x95, 0x2f, 0x88, 0x40, 0x00, 0x00,
	'5', 5, 10,	0xfc, 0x3c, 0x10, 0xc5, 0xc0, 0x00, 0x00,
	'6', 5, 10,	0x74, 0x61, 0xe8, 0xc5, 0xc0, 0x00, 0x00,
	'7', 5, 10,	0xfc, 0x44, 0x42, 0x10, 0x80, 0x00, 0x00,
	'8', 5, 10,	0x74, 0x62, 0xe8, 0xc5, 0xc0, 0x00, 0x00,
	'9', 5, 10,	0x74, 0x62, 0xf0, 0xc5, 0xc0, 0x00, 0x00,

	'A', 6, 10,	0x31, 0x28, 0x7f, 0x86, 0x18, 0x40, 0x00, 0x00,
	'B', 6, 10,	0xfa, 0x18, 0x7e, 0x86, 0x1f, 0x80, 0x00, 0x00,
	'C', 6, 10,	0x7a, 0x18, 0x20, 0x82, 0x17, 0x80, 0x00, 0x00,
	'D', 6, 10,	0xfa, 0x18, 0x61, 0x86, 0x1f, 0x80, 0x00, 0x00,
	'E', 6, 10,	0xfe, 0x08, 0x3c, 0x82, 0x0f, 0xc0, 0x00, 0x00,
	'F', 6, 10,	0xfe, 0x08, 0x3c, 0x82, 0x08, 0x00, 0x00, 0x00,
	'G', 6, 10,	0x7a, 0x18, 0x27, 0x86, 0x17, 0xc0, 0x00, 0x00,
	'H', 6, 10,	0x86, 0x18, 0x7f, 0x86, 0x18, 0x40, 0x00, 0x00,
	'I', 3, 10,	0xe9, 0x24, 0xb8, 0x00,
	'J', 6, 10,	0xfc, 0x41, 0x04, 0x12, 0x46, 0x00, 0x00, 0x00,
	'K', 5, 10,	0x8c, 0xa9, 0x8a, 0x4a, 0x20, 0x00, 0x00,
	'L', 4, 10,	0x88, 0x88, 0x88, 0xf0, 0x00,
	'M', 6, 10,	0x87, 0x3b, 0x61, 0x86, 0x18, 0x40, 0x00, 0x00,
	'N', 5, 10,	0x8e, 0x6b, 0x38, 0xc6, 0x20, 0x00, 0x00,
	'O', 6, 10,	0x7a, 0x18, 0x61, 0x86, 0x17, 0x80, 0x00, 0x00,
	'P', 5, 10,	0xf4, 0x63, 0xe8, 0x42, 0x00, 0x00, 0x00,
	'Q', 6, 10,	0x7a, 0x18, 0x61, 0x86, 0x57, 0x81, 0x00, 0x00,
	'R', 5, 10,	0xf4, 0x63, 0xe8, 0xc6, 0x20, 0x00, 0x00,
	'S', 6, 10,	0x7a, 0x18, 0x1e, 0x06, 0x17, 0x80, 0x00, 0x00,
	'T', 3, 10,	0xe9, 0x24, 0x90, 0x00,
	'U', 6, 10,	0x86, 0x18, 0x61, 0x86, 0x17, 0x80, 0x00, 0x00,
	'V', 6, 10,	0x86, 0x18, 0x61, 0x85, 0x23, 0x00, 0x00, 0x00,
	'W', 7, 10,	0x83, 0x06, 0x4c, 0x99, 0x35, 0x51, 0x00, 0x00, 0x00,
	'X', 5, 10,	0x8c, 0x54, 0x45, 0x46, 0x20, 0x00, 0x00,
	'Y', 5, 10,	0x8c, 0x54, 0x42, 0x10, 0x80, 0x00, 0x00,
	'Z', 6, 10,	0xfc, 0x10, 0x84, 0x21, 0x0f, 0xc0, 0x00, 0x00,

	'a', 4, 10,	0x00, 0x61, 0x79, 0x70, 0x00,
	'b', 4, 10,	0x88, 0xe9, 0x99, 0xe0, 0x00,
	'c', 4, 10,	0x00, 0x78, 0x88, 0x70, 0x00,
	'd', 4, 10,	0x11, 0x79, 0x99, 0x70, 0x00,
	'e', 4, 10,	0x00, 0x69, 0xf8, 0x60, 0x00,
	'f', 4, 10,	0x25, 0x4e, 0x44, 0x40, 0x00,
	'g', 4, 10,	0x00, 0x79, 0x99, 0x71, 0x60,
	'h', 4, 10,	0x88, 0xe9, 0x99, 0x90, 0x00,
	'i', 1, 10,	0xbe, 0x00,
	'j', 2, 10,	0x04, 0x55, 0x80,
	'k', 4, 10,	0x89, 0xac, 0xca, 0x90, 0x00,
	'l', 3, 10,	0xc9, 0x24, 0x98, 0x00,
	'm', 5, 10,	0x00, 0x15, 0x5a, 0xd6, 0x20, 0x00, 0x00,
	'n', 4, 10,	0x00, 0xe9, 0x99, 0x90, 0x00,
	'o', 4, 10,	0x00, 0x69, 0x99, 0x60, 0x00,
	'p', 4, 10,	0x00, 0xe9, 0x99, 0xe8, 0x80,
	'q', 4, 10,	0x00, 0x79, 0x97, 0x11, 0x10,
	'r', 3, 10,	0x02, 0xe9, 0x20, 0x00,
	's', 4, 10,	0x00, 0x78, 0x61, 0xe0, 0x00,
	't', 3, 10,	0x4b, 0xa4, 0x88, 0x00,
	'u', 4, 10,	0x00, 0x99, 0x99, 0x70, 0x00,
	'v', 4, 10,	0x00, 0x99, 0x99, 0x60, 0x00,
	'w', 5, 10,	0x00, 0x23, 0x1a, 0xd5, 0x40, 0x00, 0x00,
	'x', 5, 10,	0x00, 0x22, 0xa2, 0x2a, 0x20, 0x00, 0x00,
	'y', 4, 10,	0x00, 0x99, 0x99, 0x71, 0x60,
	'z', 4, 10,	0x00, 0xf1, 0x24, 0xf0, 0x00,

	0,

};
#if 0
// A small font map
static const char font_map_small [] =
{
	// Default glyph
	'.', 1, 6,	0x08,

	// Tab width
	'\t', 8, 0,

	// Space
	' ', 3, 0,

	'!', 1, 6,	0xea,
	'+', 3, 6,	0x0b, 0xa0, 0x00,
	'-', 3, 6,	0x03, 0x80, 0x00,
	'/', 3, 6,	0x25, 0x48, 0x00,
	'*', 3, 6,	0xab, 0xaa, 0x00,
	'@', 4, 6,	0x69, 0xbb, 0x87,
	':', 1, 6,	0x52,
	'=', 3, 6,	0x1c, 0x70, 0x00,
	'?', 4, 6,	0x69, 0x24, 0x04,
	'%', 3, 6,	0x85, 0x28, 0x40,
	'^', 3, 6,	0x54, 0x00, 0x00,
	'#', 5, 6,	0x57, 0xd5, 0xf5, 0x00,
	'$', 5, 6,	0x23, 0xe8, 0xe2, 0xf8,
	'~', 4, 6,	0x05, 0xa0, 0x00,

	'0', 3, 6,	0x56, 0xd4, 0x31,
	'1', 2, 6,	0xd5, 0x42,
	'2', 4, 6,	0xe1, 0x68, 0xf0,
	'3', 4, 6,	0xe1, 0x61, 0xe0,
	'4', 4, 6,	0x89, 0xf1, 0x10,
	'5', 4, 6,	0xf8, 0xe1, 0xe0,
	'6', 4, 6,	0x78, 0xe9, 0x60,
	'7', 4, 6,	0xf1, 0x24, 0x40,
	'8', 4, 6,	0x69, 0x69, 0x60,
	'9', 4, 6,	0x69, 0x71, 0x60,

	'A', 4, 6,	0x69, 0xf9, 0x90,
	'B', 4, 6,	0xe9, 0xe9, 0xe0,
	'C', 4, 6,	0x78, 0x88, 0x70,
	'D', 4, 6,	0xe9, 0x99, 0xe0,
	'E', 4, 6,	0xf8, 0xe8, 0xf0,
	'F', 4, 6,	0xf8, 0xe8, 0x80,
	'G', 4, 6,	0x78, 0xb9, 0x70,
	'H', 4, 6,	0x99, 0xf9, 0x90,
	'I', 3, 6,	0xe9, 0x2e, 0x00,
	'J', 4, 6,	0xf2, 0x2a, 0x40,
	'K', 4, 6,	0x9a, 0xca, 0x90,
	'L', 3, 6,	0x92, 0x4e, 0x00,
	'M', 5, 6,	0x8e, 0xeb, 0x18, 0x80,
	'N', 4, 6,	0x9d, 0xb9, 0x90,
	'O', 4, 6,	0x69, 0x99, 0x60,
	'P', 4, 6,	0xe9, 0xe8, 0x80,
	'Q', 4, 6,	0x69, 0x9b, 0x70,
	'R', 4, 6,	0xe9, 0xea, 0x90,
	'S', 4, 6,	0x78, 0x61, 0xe0,
	'T', 3, 6,	0xe9, 0x24, 0x00,
	'U', 4, 6,	0x99, 0x99, 0x60,
	'V', 4, 6,	0x99, 0x96, 0x60,
	'W', 5, 6,	0x8c, 0x6b, 0x55, 0x00,
	'X', 4, 6,	0x99, 0x69, 0x90,
	'Y', 3, 6,	0xb5, 0x24, 0x00,
	'Z', 4, 6,	0xf2, 0x48, 0xf0,

	'a', 4, 6,	0x69, 0xf9, 0x90,
	'b', 4, 6,	0xe9, 0xe9, 0xe0,
	'c', 4, 6,	0x78, 0x88, 0x70,
	'd', 4, 6,	0xe9, 0x99, 0xe0,
	'e', 4, 6,	0xf8, 0xe8, 0xf0,
	'f', 4, 6,	0xf8, 0xe8, 0x80,
	'g', 4, 6,	0x78, 0xb9, 0x70,
	'h', 4, 6,	0x99, 0xf9, 0x90,
	'i', 3, 6,	0xe9, 0x2e, 0x00,
	'j', 4, 6,	0xf2, 0x2a, 0x40,
	'k', 4, 6,	0x9a, 0xca, 0x90,
	'l', 3, 6,	0x92, 0x4e, 0x00,
	'm', 5, 6,	0x8e, 0xeb, 0x18, 0x80,
	'n', 4, 6,	0x9d, 0xb9, 0x90,
	'o', 4, 6,	0x69, 0x99, 0x60,
	'p', 4, 6,	0xe9, 0xe8, 0x80,
	'q', 4, 6,	0x69, 0x9b, 0x70,
	'r', 4, 6,	0xe9, 0xea, 0x90,
	's', 4, 6,	0x78, 0x61, 0xe0,
	't', 3, 6,	0xe9, 0x24, 0x00,
	'u', 4, 6,	0x99, 0x99, 0x60,
	'v', 4, 6,	0x99, 0x96, 0x60,
	'w', 5, 6,	0x8c, 0x6b, 0x55, 0x00,
	'x', 4, 6,	0x99, 0x69, 0x90,
	'y', 3, 6,	0xb5, 0x24, 0x00,
	'z', 4, 6,	0xf2, 0x48, 0xf0,

	0,
};
#endif

static void _zbar_video_recycle_image (zbar_image_t *img)
{
    zbar_video_t *vdo = img->src;
    assert(vdo);
    assert(img->srcidx >= 0);
    video_lock(vdo);
    if(vdo->images[img->srcidx] != img)
        vdo->images[img->srcidx] = img;
    if(vdo->active)
        vdo->nq(vdo, img);
    else
        video_unlock(vdo);
}

static void _zbar_video_recycle_shadow (zbar_image_t *img)
{
    zbar_video_t *vdo = img->src;
    assert(vdo);
    assert(img->srcidx == -1);
    video_lock(vdo);
    img->next = vdo->shadow_image;
    vdo->shadow_image = img;
    video_unlock(vdo);
}

zbar_video_t *zbar_video_create ()
{
    zbar_video_t *vdo = calloc(1, sizeof(zbar_video_t));
    int i;
    if(!vdo)
        return(NULL);
    err_init(&vdo->err, ZBAR_MOD_VIDEO);
    vdo->fd = -1;

    (void)_zbar_mutex_init(&vdo->qlock);

    /* pre-allocate images */
    vdo->num_images = ZBAR_VIDEO_IMAGES_MAX;
    vdo->images = calloc(ZBAR_VIDEO_IMAGES_MAX, sizeof(zbar_image_t*));
    if(!vdo->images) {
        zbar_video_destroy(vdo);
        return(NULL);
    }

    for(i = 0; i < ZBAR_VIDEO_IMAGES_MAX; i++) {
        zbar_image_t *img = vdo->images[i] = zbar_image_create();
        if(!img) {
            zbar_video_destroy(vdo);
            return(NULL);
        }
        img->refcnt = 0;
        img->cleanup = _zbar_video_recycle_image;
        img->srcidx = i;
        img->src = vdo;
    }

    return(vdo);
}

void zbar_video_destroy (zbar_video_t *vdo)
{
    if(vdo->intf != VIDEO_INVALID)
        zbar_video_open(vdo, NULL);
    if(vdo->images) {
        int i;
        for(i = 0; i < ZBAR_VIDEO_IMAGES_MAX; i++)
            if(vdo->images[i])
                _zbar_image_free(vdo->images[i]);
        free(vdo->images);
    }
    while(vdo->shadow_image) {
        zbar_image_t *img = vdo->shadow_image;
        vdo->shadow_image = img->next;
        free((void*)img->data);
        img->data = NULL;
        free(img);
    }
    if(vdo->buf)
        free(vdo->buf);
    if(vdo->formats)
        free(vdo->formats);
    if(vdo->emu_formats)
        free(vdo->emu_formats);

    if(vdo->free)
        vdo->free(vdo);

    err_cleanup(&vdo->err);
    _zbar_mutex_destroy(&vdo->qlock);

#ifdef HAVE_LIBJPEG
    if(vdo->jpeg_img) {
        zbar_image_destroy(vdo->jpeg_img);
        vdo->jpeg_img = NULL;
    }
    if(vdo->jpeg) {
        _zbar_jpeg_decomp_destroy(vdo->jpeg);
        vdo->jpeg = NULL;
    }
#endif
    free(vdo);
}

int zbar_video_open (zbar_video_t *vdo,
                     const char *dev)
{
    char *ldev = NULL;
    int rc;
    zbar_video_enable(vdo, 0);
    video_lock(vdo);
    if(vdo->intf != VIDEO_INVALID) {
        if(vdo->cleanup) {
            vdo->cleanup(vdo);
            vdo->cleanup = NULL;
        }
        zprintf(1, "closed camera (fd=%d)\n", vdo->fd);
        vdo->intf = VIDEO_INVALID;
    }
    video_unlock(vdo);

    if(!dev)
        return(0);

    if((unsigned char)dev[0] < 0x10) {
        /* default linux device, overloaded for other platforms */
        int id = dev[0];
        dev = ldev = strdup("/dev/video0");
        ldev[10] = '0' + id;
    }

    rc = _zbar_video_open(vdo, dev);

    if(ldev)
        free(ldev);
    return(rc);
}

int zbar_video_get_fd (const zbar_video_t *vdo)
{
    if(vdo->intf == VIDEO_INVALID)
        return(err_capture(vdo, SEV_ERROR, ZBAR_ERR_INVALID, __func__,
                           "video device not opened"));
    if(vdo->intf != VIDEO_V4L2)
        return(err_capture(vdo, SEV_WARNING, ZBAR_ERR_UNSUPPORTED, __func__,
                           "video driver does not support polling"));
    return(vdo->fd);
}

int zbar_video_request_size (zbar_video_t *vdo,
                             unsigned width,
                             unsigned height)
{
    if(vdo->initialized)
        /* FIXME re-init different format? */
        return(err_capture(vdo, SEV_ERROR, ZBAR_ERR_INVALID, __func__,
                           "already initialized, unable to resize"));

    vdo->width = width;
    vdo->height = height;
    zprintf(1, "request size: %d x %d\n", width, height);
    return(0);
}

int zbar_video_request_interface (zbar_video_t *vdo,
                                  int ver)
{
    if(vdo->intf != VIDEO_INVALID)
        return(err_capture(vdo, SEV_ERROR, ZBAR_ERR_INVALID, __func__,
                         "device already opened, unable to change interface"));
    vdo->intf = (video_interface_t)ver;
    zprintf(1, "request interface version %d\n", vdo->intf);
    return(0);
}

int zbar_video_request_iomode (zbar_video_t *vdo,
                               int iomode)
{
    if(vdo->intf != VIDEO_INVALID)
        return(err_capture(vdo, SEV_ERROR, ZBAR_ERR_INVALID, __func__,
                         "device already opened, unable to change iomode"));
    if(iomode < 0 || iomode > VIDEO_USERPTR)
        return(err_capture(vdo, SEV_ERROR, ZBAR_ERR_INVALID, __func__,
                         "invalid iomode requested"));
    vdo->iomode = iomode;
    return(0);
}

int zbar_video_get_width (const zbar_video_t *vdo)
{
    return(vdo->width);
}

int zbar_video_get_height (const zbar_video_t *vdo)
{
    return(vdo->height);
}

uint32_t zbar_video_get_format (const zbar_video_t *vdo)
{
    return(vdo->format);
}

static inline int video_init_images (zbar_video_t *vdo)
{
    int i;
    assert(vdo->datalen);
    if(vdo->iomode != VIDEO_MMAP) {
        assert(!vdo->buf);
        vdo->buflen = vdo->num_images * vdo->datalen;
        vdo->buf = calloc(1, vdo->buflen);
        if(!vdo->buf)
            return(err_capture(vdo, SEV_FATAL, ZBAR_ERR_NOMEM, __func__,
                               "unable to allocate image buffers"));
        zprintf(1, "pre-allocated %d %s buffers size=0x%lx\n", vdo->num_images,
                (vdo->iomode == VIDEO_READWRITE) ? "READ" : "USERPTR",
                vdo->buflen);
    }
    for(i = 0; i < vdo->num_images; i++) {
        zbar_image_t *img = vdo->images[i];
        img->format = vdo->format;
        zbar_image_set_size(img, vdo->width, vdo->height);
        if(vdo->iomode != VIDEO_MMAP) {
            unsigned long offset = i * vdo->datalen;
            img->datalen = vdo->datalen;
            img->data = (uint8_t*)vdo->buf + offset;
            zprintf(2, "    [%02d] @%08lx\n", i, offset);
        }
    }
    return(0);
}

int zbar_video_init (zbar_video_t *vdo,
                     unsigned long fmt)
{
#ifdef HAVE_LIBJPEG
    const zbar_format_def_t *vidfmt;
#endif
    if(vdo->initialized)
        /* FIXME re-init different format? */
        return(err_capture(vdo, SEV_ERROR, ZBAR_ERR_INVALID, __func__,
                           "already initialized, re-init unimplemented"));

    if(vdo->init(vdo, fmt))
        return(-1);
    vdo->format = fmt;
    if(video_init_images(vdo))
        return(-1);
#ifdef HAVE_LIBJPEG
    vidfmt = _zbar_format_lookup(fmt);
    if(vidfmt && vidfmt->group == ZBAR_FMT_JPEG) {
        zbar_image_t *img;
        /* prepare for decoding */
        if(!vdo->jpeg)
            vdo->jpeg = _zbar_jpeg_decomp_create();
        if(vdo->jpeg_img)
            zbar_image_destroy(vdo->jpeg_img);

        /* create intermediate image for decoder to use*/
        img = vdo->jpeg_img = zbar_image_create();
        img->format = fourcc('Y','8','0','0');
        zbar_image_set_size(img, vdo->width, vdo->height);
        img->datalen = vdo->width * vdo->height;
    }
#endif
    vdo->initialized = 1;
    return(0);
}

int zbar_video_enable (zbar_video_t *vdo,
                       int enable)
{
    if(vdo->active == enable)
        return(0);

    if(enable) {
        if(vdo->intf == VIDEO_INVALID)
            return(err_capture(vdo, SEV_ERROR, ZBAR_ERR_INVALID, __func__,
                               "video device not opened"));

        if(!vdo->initialized &&
           zbar_negotiate_format(vdo, NULL))
            return(-1);
    }

    if(video_lock(vdo))
        return(-1);
    vdo->active = enable;
    if(enable) {
        /* enqueue all buffers */
        int i;
        for(i = 0; i < vdo->num_images; i++)
            if(vdo->nq(vdo, vdo->images[i]) ||
               ((i + 1 < vdo->num_images) && video_lock(vdo)))
                return(-1);
        
        return(vdo->start(vdo));
    }
    else {
        int i;
        for(i = 0; i < vdo->num_images; i++)
            vdo->images[i]->next = NULL;
        vdo->nq_image = vdo->dq_image = NULL;
        if(video_unlock(vdo))
            return(-1);

        return(vdo->stop(vdo));
    }
}

const char* ezd_next_glyph( const char* pGlyph )
{
	int sz;

	// Last glyph?
	if ( !pGlyph || !*pGlyph )
		return 0;

	// Glyph size in bits
	sz = pGlyph[ 1 ] * pGlyph[ 2 ];

	// Return a pointer to the next glyph
	return &pGlyph[ 3 + ( ( sz & 0x07 ) ? ( ( sz >> 3 ) + 1 ) : sz >> 3 ) ];
}

const char* ezd_find_glyph(const char* x_pFt, const char ch )
{
    const char* pGlyph = x_pFt;

	// Find the glyph
	while ( pGlyph && *pGlyph )
		if ( ch == *pGlyph )
			return pGlyph;
		else
			pGlyph = ezd_next_glyph( pGlyph );

	// First glyph is the default
	return (const char*)x_pFt;
}

//modified from ezdib
void ezd_draw_bmp_8( unsigned char *pImg, int x, int y, int sw, int pw,int bw, int bh, const char *pBmp)
{
	int w, h, lx = x;
	unsigned char m = 0x80;

	// Draw the glyph
	for( h = 0; h < bh; h++ )
	{
		// Draw horz line
		for( w = 0; w < bw; w++ )
		{
			// Next glyph byte?
			if ( !m )
				m = 0x80, pBmp++;

			// Is this pixel on?
			if ( *pBmp & m ) {
                pImg[y * sw + lx ] = 0xff;
            }

			// Next bmp bit
			m >>= 1;

			// Next x pixel
			lx++;

		} // end for

		// Reset x
		lx = x;

		// Reset y
		y++;

	} // end for

}

void ezd_text(unsigned char *img, char *x_pText, int x_nTextLen, int x, int y) {
    int w = 320, h = 240, sw = 320, pw = 1, i, mh = 0, lx = x;
    const char *pGlyph;

    for ( i = 0; i < x_nTextLen || ( 0 > x_nTextLen && x_pText[ i ] ); i++ )
	{
		// Get the specified glyph
		pGlyph = ezd_find_glyph( font_map_medium, x_pText[ i ] );

		// Other characters
        // Draw this glyph if it's completely on the screen
        if ( pGlyph[ 1 ] && pGlyph[ 2 ]
                && 0 <= lx && ( lx + pGlyph[ 1 ] ) < w
                && 0 <= y && ( y + pGlyph[ 2 ] ) < h )
        {
            ezd_draw_bmp_8(img, lx, y, sw, pw, pGlyph[ 1 ], pGlyph[ 2 ], &pGlyph[ 3 ]);
        } // end if

        // Next character position
        lx += 2 + pGlyph[ 1 ];

        // Track max height
        mh = ( pGlyph[ 2 ] > mh ) ? pGlyph[ 2 ] : mh;
	} // end for
}

ssize_t write(int fd, const void *buf, size_t count);
void chobits_write_frame(zbar_video_t *video, zbar_image_t *img, char* symbol_data, int x, int y) {
    if (x >= 0) {
        //printf("chobits :%s\n", symbol_data);
        ezd_text(img->data, symbol_data, -1, x, y);
    }
    write(video->o_fd, img->data, img->datalen);
}

zbar_image_t *zbar_video_next_image (zbar_video_t *vdo)
{
    unsigned frame;
    zbar_image_t *img;

    if(video_lock(vdo))
        return(NULL);
    if(!vdo->active) {
        video_unlock(vdo);
        return(NULL);
    }

    frame = vdo->frame++;
    img = vdo->dq(vdo);
    if(img) {
        img->seq = frame;
        if(vdo->num_images < 2) {
            /* return a *copy* of the video image and immediately recycle
             * the driver's buffer to avoid deadlocking the resources
             */
            zbar_image_t *tmp = img;
            video_lock(vdo);
            img = vdo->shadow_image;
            vdo->shadow_image = (img) ? img->next : NULL;
            video_unlock(vdo);
                
            if(!img) {
                img = zbar_image_create();
                assert(img);
                img->refcnt = 0;
                img->src = vdo;
                /* recycle the shadow images */

                img->format = vdo->format;
                zbar_image_set_size(img, vdo->width, vdo->height);
                img->datalen = vdo->datalen;
                img->data = malloc(vdo->datalen);
            }
            img->cleanup = _zbar_video_recycle_shadow;
            img->seq = frame;
            memcpy((void*)img->data, tmp->data, img->datalen);
            _zbar_video_recycle_image(tmp);
        }
        else
            img->cleanup = _zbar_video_recycle_image;
        _zbar_image_refcnt(img, 1);
    }
    return(img);
}

/** @brief return if fun unsupported, otherwise continue */
#define return_if_not_supported(fun, name) \
{ \
    if(!(fun)) { \
        zprintf(1, "video driver does not implement %s\n", name); \
        return ZBAR_ERR_UNSUPPORTED; \
    } \
}
#define return_if_non_zero(a) { int rv=a; if (rv!=0) return(rv); }

int zbar_video_set_control (zbar_video_t *vdo,
                            const char *control_name,
                            int value)
{
    int loc_value, rv;
    return_if_not_supported(vdo->set_control, "set_control");
    loc_value = value;

    rv = vdo->set_control(vdo, control_name, &loc_value);

    if(rv==0)
        zprintf(1, "value of %s set to: %d\n", control_name, loc_value);
    return(rv);
}

int zbar_video_get_control (zbar_video_t *vdo,
                            const char *control_name,
                            int *value)
{
    return_if_not_supported(vdo->get_control, "get_control");
    return(vdo->get_control(vdo, control_name, value));
}

struct video_controls_s *zbar_video_get_controls (const zbar_video_t *vdo,
                                                  int index)
{
    int i = 0;
    struct video_controls_s *p = vdo->controls;

    while (p && i != index) {
        i++;
        p = p->next;
    }

    if (!p)
        return NULL;

    return p;
}

struct video_resolution_s *zbar_video_get_resolutions (const zbar_video_t *vdo,
                                                       int index)
{
    int i = 0;
    struct video_resolution_s *p = vdo->res;

    while (i != index) {
        if (!p->width || !p->height)
            return NULL;
        i++;
        p++;
    }

    if (!p->width || !p->height)
       return NULL;

    return p;
}
