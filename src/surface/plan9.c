/*
 * Copyright 2009 Vincent Sanders <vince@simtec.co.uk>
 *
 * This file is part of libnsfb, http://www.netsurf-browser.org/
 * Licenced under the MIT License,
 *                http://www.opensource.org/licenses/mit-license.php
 */

#define _PLAN9_SOURCE
#define TIMEOUT_MILLISEC 1000


#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <ctype.h>

#include "libnsfb.h"
#include "libnsfb_event.h"
#include "libnsfb_plot.h"
#include "libnsfb_plot_util.h"

#include "nsfb.h"
#include "surface.h"
#include "plot.h"
#include "cursor.h"

/* Plan 9 includes */

#include <draw.h>
#include <event.h>

static bool inited;
static int gwidth;
static int gheight;
static bool perform_resize;

static unsigned char *
create_local_image(int bytes);
Image *
create_draw_image(int width, int height, ulong chan);

/*
 * Functions and structures for buffering of events
 *
 * There is no 1:1 relationship between Plan 9 events and
 * NSFB events (nsevent). The following ringbuffer can
 * ease the translation of one Plan 9 keyevent to multiple
 * nsevent or one mouse event to multiple nsevents for
 * mousebutton press/release events and movement events
 *
 */
#define EVBUFSIZE 8		/* max no. of events buffered */

typedef struct eventbuffer_s {
	nsfb_event_t buffer[EVBUFSIZE];
	int buflen;		/* how many events are in the buffer */
	int readidx;		/* next free slot to add event */
	int writeidx;		/* first available event to read */
} eventbuffer_t;

/**
 * Put an nsevent on hold (fifo buffer ringbuffer)
 * 
 * \param evbuf		ptr to the buffer (typically in drawstate)
 * \param nsevent	ptr to event to buffer
 * \return		0 if buffer is full, 1 for success.
 *
 */

int
putevent(eventbuffer_t *evbuf, nsfb_event_t *nsevent)
{
	if(evbuf->buflen >= EVBUFSIZE){
		return 0;
	}
	evbuf->buffer[evbuf->writeidx++] = *nsevent;
	evbuf->buflen++;
	if(evbuf->writeidx >= EVBUFSIZE)
		evbuf->writeidx = 0;
	return 1;
}

/**
 * Get an nsevent from the bufffer (fifo ringbuffer)
 * 
 * \param evbuf		ptr to the buffer (typically in drawstate)
 * \param nsevent	ptr to event to write to
 * \return		0 if buffer is empty, 1 for success.
 *
 */

int
getevent(eventbuffer_t *evbuf, nsfb_event_t *nsevent)
{
	if(evbuf->buflen < 1){
		return 0;	/* fail (emtpy) */
	}
	*nsevent = evbuf->buffer[evbuf->readidx++];
	evbuf->buflen--;
	if(evbuf->readidx >= EVBUFSIZE)
		evbuf->readidx = 0;

	return 1;		/* success */
}


/*
 * A 'drawstate' contain all information about the
 * the connecton to graphics in Plan 9 as well a pointer
 * to the memory area in which the library updates the content.
 */

typedef struct drawstate_s {
	unsigned char *localimage;	/* common buffer (client) */
	unsigned char *updateimage;	/* part of the image to update */
	Image *srvimage;		/* dispaly buffer (server) */
	int imagebytes;			/* buffer size in bytes */
	int mousebuttons;		/* last mouse button status */
	eventbuffer_t eventbuffer;	/* buffer of incoming events */
} drawstate_t;


/* Posix sleep() sleeps in seconds, and plan 9's sleep() that sleeps
 * in milliseconds is not easily accessible in APE, so mssleep() is
 * used to sleep a number of milliseconds, calling Posix nanosleep().
 */

int
mssleep(int ms)		/* sleep milliseconds */
{
	struct timespec req, rem;
	if (ms > 999) {
		req.tv_sec = (int) (ms/1000);
		req.tv_nsec = (ms - ((long)req.tv_sec * 1000)) * 1000000;
	} else {
		req.tv_sec = 0;
		req.tv_nsec = ms * 1000000;
	}
	return nanosleep(&req, &rem);	
}


/* I am not sure if this routine is needed to be implemented.
 * I think it makes a copy of the display if the resolution is
 * changed on the fly. But I am not sure that is even supported
 * in framebuffer mode
 */

static bool
p9copy(nsfb_t *nsfb, nsfb_bbox_t *srcbox, nsfb_bbox_t *dstbox)
{
	Point srcpt;
	Rectangle dstrect;
	drawstate_t *drawstate = nsfb->surface_priv;
	srcpt.x=srcbox->x0;
	srcpt.y=srcbox->y0;
	dstrect.min.x = dstbox->x0;
	dstrect.min.y = dstbox->y0;
	dstrect.max.x = dstbox->x1;
	dstrect.max.y = dstbox->y1;
	draw(drawstate->srvimage,
		dstrect, 
		drawstate->srvimage, 
		nil, 
		srcpt);
	return true;
}


static int 
plan9_set_geometry(nsfb_t *nsfb, int width, int height,
		enum nsfb_format_e format)
{
	if(!inited) {
		fprintf(stderr, "INITING display!\n");
		if (initdraw(0, 0, "netsurf-fb") < 0){
			fprintf(stderr, "initdraw failed\n");
			return -1;
		}
		inited=true;
	}
	//fprintf(stderr, "DBG: plan9_set_geometry(%d,%d) - check p9copy()!\n",
	//		width, height);
	
	nsfb->width = width;
	nsfb->height = height;
	nsfb->format = format;

	gwidth=width;
	gheight=height;

	/* select default sw plotters for format */
	select_plotters(nsfb);
	nsfb->plotter_fns->copy = p9copy;	/* empty function */

	drawstate_t *drawstate = nsfb->surface_priv;

	/* sanity check bpp. */
	if ((nsfb->bpp != 32) && (nsfb->bpp != 16) && (nsfb->bpp != 8))
		return -1;

	if (drawstate == NULL)
		drawstate = calloc(1, sizeof(drawstate_t));
	if (drawstate == NULL)
		return -1;	/* no memory */

	/* create local framebuffer data storage */
	drawstate->imagebytes =
		(nsfb->bpp * nsfb->width * nsfb->height) >> 3;

	if(drawstate->localimage) free(drawstate->localimage);
	drawstate->localimage = calloc(1, drawstate->imagebytes); //create_local_image(drawstate->imagebytes);
	if(drawstate->updateimage) free(drawstate->updateimage);
	drawstate->updateimage = calloc(1, drawstate->imagebytes); //create_local_image(drawstate->imagebytes);

	if (drawstate->localimage == NULL || drawstate->updateimage == NULL){
		fprintf(stderr, "Unable to allocate memory "
				"for local framebuffer images\n");
		free(drawstate);
		return -1;
		//drawshutdown(); /* to call this? */
	}

	/* crate a draw image on server side */
	drawstate->srvimage = create_draw_image(nsfb->width,
			nsfb->height, XRGB32);

	if (drawstate->srvimage == NULL){
		fprintf(stderr, "Unable to create an image "
				"on the display server\n");
		free(drawstate->localimage);
		free(drawstate->updateimage);
		free(drawstate);
		return -1;
		//drawshutdown(); /* to call this? */
	}	
	
	/* ensure plotting information is stored */
	nsfb->surface_priv = drawstate;
	nsfb->ptr = drawstate->localimage;
	nsfb->linelen = (nsfb->width * nsfb->bpp) / 8;

	return 0;
}


void
eresized(int new)		/* callback also called by libdraw */
{
	perform_resize=true;
	if (new && getwindow(display, Refmesg) < 0)
		fprintf(stderr,"can't reattach to window");	
}

/* create_local_image()
 *
 *	Allocate a frame buffer in user space memory, that 
 *	the rest of the framebuffer library can write to.
 *	The contents has to be loaded from here to the server
 *	image, when it is updated.
 */

static unsigned char *
create_local_image(int bytes)
{
	unsigned char *image_data;

//	fprintf(stderr, "DBG: create_local_image(%d) -> %d KB\n",
//			bytes, bytes>>10);

	image_data = calloc(1, bytes);
	if (image_data == NULL)
		return NULL;

	return image_data;
}

/* create_draw_image()
 *
 *	Creates a Plan 9 'Image' object on the display server.
 */

Image *
create_draw_image(int width, int height, ulong chan)
{
	Rectangle r;

//	fprintf(stderr, "DBG: create_draw_image(%d,%d, ch=%x)\n",
//			width, height, chan);
	
/*	if(bpp != 24)
		return NULL;	*/	/* is this needed? */

	r.min.x = 0;
	r.min.y = 0;
	r.max.x = width;
	r.max.y = height;

	return allocimage(display, r, chan, 0, DWhite);
}


static int
plan9_initialise(nsfb_t *nsfb)
{
	if(!inited)	/* if we are called before plan9_set_geometry() */
		plan9_set_geometry(nsfb, nsfb->width, nsfb->height, nsfb->format);

	einit(Emouse|Ekeyboard);
	eresized(0);	/* first drawing */
	return 0;
}

static int plan9_finalise(nsfb_t *nsfb)
{
	drawstate_t *drawstate = nsfb->surface_priv;

//	fprintf(stderr, "DBG: plan9_finalise()\n");

	if (drawstate == NULL)
		return 0;
	/* free local image */
	/* --- should free allocated structures here --- */
	/* disconnect from display server? */
	return 0;
}

/* wait_event()		Waits about 'timeout' milliseconds for an
 *			event. Returns 1 if there is an event, and
 *			0 if timed out.
 */

static int
wait_event(int timeout)
{
	int i, steps;

	steps = timeout / 250;

	for(i=0; i< steps; i++){
		if(ecanread(Ekeyboard|Emouse))
			return 1;	/* event available */
		mssleep(250);
	}
	return 0;	/* timed out */
}

/* convert from plan9 keyboard codes (runes) to NSFB keycodes */
/* currently only handling A-Z and some special keys */
static int
plan9_to_nsfbkeycode(Event *evp)
{
	int key;	/* plan 9 key */
	int code;	/* NSFB code */

	key = evp->kbdc;

//	fprintf(stderr, "DBG: kbdc = %d [%c]\n", key, key);

	if (32 <= key && key <= 127) 		/* space to DEL */
		code = key;
	else if (8 <= key && key <= 9)		/* BS, TAB */
		code = key;
	else if (key == 10)
		code = NSFB_KEY_RETURN;		/* LF -> CR */
	else if (key == 0xf011)
		code = NSFB_KEY_LEFT;
	else if (key == 0xf012)
		code = NSFB_KEY_RIGHT;
	else
		code = NSFB_KEY_UNKNOWN;

	return code;
}

/* button_changed()	Check if mouse button 'butnum' (1,2,3)
 *			has been pressed or released since the
 *			butto recording.
 *
 * returns		MSAME, MDOWN or MUP
 */

enum { MSAME = 0, MDOWN = 1, MUP = 2 };

static int
button_changed(int newbuttons, int oldbuttons, int butnum)
{
	int mask;

	mask = 1 << butnum-1;	/* mask is 1,2,4 for buttons  1,2,3 */

	if	(!(oldbuttons & mask) &&  (newbuttons & mask))
			return MDOWN;
	else if	( (oldbuttons & mask) && !(newbuttons & mask))
			return MUP;
	else
			return MSAME;
}


/*
 * Some ascii characters (key strokes) are not recognised as events
 * themselves, but have to be sent as a SHIFTDN + unshifted key + SHIFTUP.
 * This function gives the unshifted ascii key for a keycode, if it is in
 * the list of 'problematic' keys, that are not recognised.
 * Example: '%' -> '5'. (Persent sign is shifted '5' key)
 *
 * \param	ascii	keycode to check
 * \return		0 if not a special symbol, otherwhise the ascii of
 *			the unshifted key on the british keyboard layout.
 */

int
unshiftedkeycode(int ascii)
{
	switch(ascii) {
	case '%':
		return '5';
		break;
	case '|':
		return '\\';
		break;
	case '^':
		return '6';
		break;
	case '~':
		return '#';
		break;
	case '{':
		return '[';
		break;
	case '}':
		return ']';
		break;
	}
	return 0;
}

/* trans_plan9_event()	Translate Plan 9 events (keyboard and mouse)
 *			to corresponding NSFB-events (see libnsfb_event.h).
 *
 *			If the user presses two mouse buttons at the same
 *			time, and they end up in the same event, only one
 *			of them register. Don't know if that can happen
 *			(maybe that will yield two events).
 *
 *			Also, the Plan 9 mouse event contains both movement
 *			and button information in the same event, but NSFB
 *			uses two differents event types for movement and
 *			for presses/realeases. [As there is now buffering
 *			of keyboard events, it would be easy to do for the
 *			mouse too]. The current solution will prioritise
 *			button changes, ignoring any movement happeing
 *			during a button state change. Movements are absolute
 *			and tend to come in swarms, so this should not be
 *			big problem.
 *
 *			Captital letters, and some symbols, will also
 *			generate LSHIFT events around them.
 */

static void
trans_plan9_event(nsfb_t *nsfb, nsfb_event_t *nsevent, Event *evp, int e)
{
	drawstate_t *drawstate = nsfb->surface_priv;
			/* keeping old mouse button status in drawstate */
	int chg;	/* mouse button change (MSAME|MDOWN/MUP) */
	nsevent->type = NSFB_EVENT_NONE;	/* default to NONE */
	int button_changes;	/* no. of button state chnges since last mouse event */
	int keycode;	/* nsfb keycode, converted from Plan 9 key code */
	int unshiftkey;	/* some symbols has to be sent as SHIFTD + unshiftkey + SHIFTU */

//	fprintf(stderr, "DBG: trans_plan9_event(e == %d)\n", e);

	switch (e) {
	case Ekeyboard:
		keycode = plan9_to_nsfbkeycode(evp);

		/* first event is 'passed through' and the others a buffered */

		/* check if it is a special symbol that must be sent 'shifted' */
		if(unshiftkey = unshiftedkeycode(keycode)) { /* for some special symbols */
			nsevent->type = NSFB_EVENT_KEY_DOWN;	/* event 2: key down */
			nsevent->value.keycode = unshiftkey;
			putevent(&drawstate->eventbuffer, nsevent);

			nsevent->type = NSFB_EVENT_KEY_UP;	/* event 3: key up */
			nsevent->value.keycode = unshiftkey;
			putevent(&drawstate->eventbuffer, nsevent);

			nsevent->type = NSFB_EVENT_KEY_UP;	/* event 4: SHIFT up */
			nsevent->value.keycode = NSFB_KEY_LSHIFT;
			putevent(&drawstate->eventbuffer, nsevent);

			nsevent->type = NSFB_EVENT_KEY_DOWN;	/* event 1: SHIFT down */
			nsevent->value.keycode = NSFB_KEY_LSHIFT;
		}
		else if(isupper(keycode)){
			nsevent->type = NSFB_EVENT_KEY_DOWN;	/* event 2: key down */
			nsevent->value.keycode = tolower(keycode);
			putevent(&drawstate->eventbuffer, nsevent);

			nsevent->type = NSFB_EVENT_KEY_UP;	/* event 3: key up */
			nsevent->value.keycode = tolower(keycode);
			putevent(&drawstate->eventbuffer, nsevent);

			nsevent->type = NSFB_EVENT_KEY_UP;	/* event 4: SHIFT up */
			nsevent->value.keycode = NSFB_KEY_LSHIFT;
			putevent(&drawstate->eventbuffer, nsevent);

			nsevent->type = NSFB_EVENT_KEY_DOWN;	/* event 1: SHIFT down */
			nsevent->value.keycode = NSFB_KEY_LSHIFT;
			
		} else {	/* lower case */
			nsevent->type = NSFB_EVENT_KEY_UP;	/* event 2: key up */
			nsevent->value.keycode = keycode;
			putevent(&drawstate->eventbuffer, nsevent);

			nsevent->type = NSFB_EVENT_KEY_DOWN;	/* event 1: key down */
			nsevent->value.keycode = keycode;
		}
		break;
	case Emouse:
		button_changes = 0;	/* no button chanes we know of so far... */

//		fprintf(stderr, "DBG: mouse event buttons=%d, xy=(%d,%d)\n",
//				evp->mouse.buttons,
//				evp->mouse.xy.x,
//				evp->mouse.xy.y);

		if(chg=button_changed(evp->mouse.buttons, drawstate->mousebuttons, 1)) {
			nsevent->value.keycode = NSFB_KEY_MOUSE_1;
			if(chg==MDOWN)
				nsevent->type = NSFB_EVENT_KEY_DOWN;
			else
				nsevent->type = NSFB_EVENT_KEY_UP;
			button_changes++;
		}
		if(chg=button_changed(evp->mouse.buttons, drawstate->mousebuttons, 2)) {
			nsevent->value.keycode = NSFB_KEY_MOUSE_2;
			if(chg==MDOWN)
				nsevent->type = NSFB_EVENT_KEY_DOWN;
			else
				nsevent->type = NSFB_EVENT_KEY_UP;
			button_changes++;
		}
		if(chg=button_changed(evp->mouse.buttons, drawstate->mousebuttons, 3)) {
			nsevent->value.keycode = NSFB_KEY_MOUSE_3;
			if(chg==MDOWN)
				nsevent->type = NSFB_EVENT_KEY_DOWN;
			else
				nsevent->type = NSFB_EVENT_KEY_UP;
			button_changes++;
		}
		if(evp->mouse.buttons & 8) {
			nsevent->value.keycode = NSFB_KEY_MOUSE_4;
			nsevent->type = NSFB_EVENT_KEY_DOWN;
			button_changes++;
		}
		if(evp->mouse.buttons & 16) {
			nsevent->value.keycode = NSFB_KEY_MOUSE_5;
			nsevent->type = NSFB_EVENT_KEY_DOWN;
			button_changes++;
		}
		/* save new button status, for next event to compare with */
		drawstate->mousebuttons = evp->mouse.buttons;

		if(button_changes > 0)		/* don't send motion data if there are	*/
			break;			/* button changes to take care of	*/

		/* If we got an Emouse event without mouse button state change, we'll	*/
		/* give back a motion event to NSFB instead.				*/

		nsevent->type = NSFB_EVENT_MOVE_ABSOLUTE;
		nsevent->value.vector.x = evp->mouse.xy.x - screen->r.min.x;
		nsevent->value.vector.y = evp->mouse.xy.y - screen->r.min.y;
		nsevent->value.vector.z = 0;

		break;
	}

	return;
}

/* print debugging info about a keyboard/mouse event */

void
debug_event(nsfb_event_t *nsevent, Event *evp)
{
	if (nsevent->type == NSFB_EVENT_KEY_DOWN || nsevent->type == NSFB_EVENT_KEY_UP)
		fprintf(stderr, "DBG: keycode %d (type = %d, kbdc=%d)\n",
				nsevent->value.keycode,
				nsevent->type,
				evp->kbdc);
	else if(nsevent->type == NSFB_EVENT_MOVE_ABSOLUTE)
		fprintf(stderr, "DBG: mouse (%d,%d) [screen r.min = (%d,%d)]\n",
				nsevent->value.vector.x,
				nsevent->value.vector.y,
				screen->r.min.x,
				screen->r.min.y);
}

/* plan9_input()
 *
 *	Main entry point for checking for events. It has a lot of dead
 *	code, as I didn't manage to get event timeouts to work
 *	properly, but I still have hope I will.
 */

static bool
plan9_input(nsfb_t *nsfb, nsfb_event_t *nsevent, int timeout)
{
	if(perform_resize) {
		perform_resize=false;
		int w = screen->r.max.x - screen->r.min.x;
		int h = screen->r.max.y - screen->r.min.y;
		fprintf(stderr, "RESIZE_EVENT.\n");
		nsevent->type = NSFB_EVENT_RESIZE;
		nsevent->value.resize.w = w;
		nsevent->value.resize.h = h;
		return true;
	}
	drawstate_t *drawstate = nsfb->surface_priv;
//	static int once = 0;	/* ensure etimer() is only called once */
	int e;			/* type of event */
	Event ev;		/* Plan 9 event struct */
//	static int timer_id;		/* to identify a timer event */

//	fprintf(stderr, "DBG: plan9_input(timeout = %d)\n", timeout);

	if (drawstate == NULL)
		return false;
	
//	if (!once) {		/* start the timer */
//		timer_id = etimer(0, TIMEOUT_MILLISEC);
//		fprintf(stderr, "DBG: plan9_input: timer_id is %d\n",
//				timer_id);
//		once++;
//	}
	
	/*
	 * Check if there are buffered events from pending
	 * This happens if an earlier Plan 9 event got translated
	 * into multiple nsevent. If there are at least one
	 * waiting event return the first one */

	if(getevent(&drawstate->eventbuffer, nsevent))
		return true;	/* event is filled in nsevent */


	/* Event checking behaviour depeding on 'timeout':
	 * timeout == 0	Check if there is an kbd/mouse event,
	 * 		if not, return false.
	 * timeout > 0	Wait for a kbd/mouse event, but return
	 *		false if a timer event occurs before. (*)
	 * timeout < 0	Wait for next kbd/mouse event, ignoring
	 *		any timer events.
	 *
	 * (*) CURRENTLY sleep a bit and then return if no event.
	 */

	if (timeout == 0) {
		if(!ecanread(Ekeyboard|Emouse /* | timer_id */))
			return false;	/* no event to read */
		e = event(&ev);
	//	if(e == timer_id)	/* cannot happen as there is no timer event */
	//		return false;
	} else if (timeout > 0) {
		/* solution using a timer event (not working) */
		//	e = event(&ev);
		//	if(e == timer_id)
		//		return false;
		//
		/* quick and dirty solution with sleep */
		if (wait_event(timeout)) {
			e = event(&ev);
		} else {
			nsevent->type = NSFB_EVENT_CONTROL;
			nsevent->value.controlcode = NSFB_CONTROL_TIMEOUT;
			return true;
		}
	} else {
	//	while( (e=event(&ev)) == timer_id)
	//		;
		e=event(&ev);	/* only real events at the moment */
	}
	
	/* from here on we have a keyboard or mouse event in (e, ev) */	
	
	/* this updates 'nsevent' with info on the kbd|mouse event */
	trans_plan9_event(nsfb, nsevent, &ev, e);

//	debug_event(nsevent, &ev);	/* print debug info */

	return true;	/* event was sucessfully registred */
}

/* This has something to do with the mouse pointer. Not sure if
 * it is needed, as plan 9 has its own pointer
 */
static int
plan9_claim(nsfb_t *nsfb, nsfb_bbox_t *box)
{
//	fprintf(stderr, "DBG: plan9_claim()\n");
	return 0;
}

static int
plan9_cursor(nsfb_t *nsfb, struct nsfb_cursor_s *cursor)
{
//	fprintf(stderr, "DBG: plan9_cursor()\n");
	return true;
}

/* buffer_offset()
 *
 * 	Calculate the byte offset in the locally stored
 *	image buffer for a Point in the window.
 */

int buffer_offset(Point pt, int width, int bpp)
{
	return ((pt.y * width + pt.x) * bpp) >> 3;
}

/* rect_bytes()
 *
 * 	Caluculate the number of bytes the data of a Rectangle
 *	occupies, given the number of bitplanes.
 */

int rect_bytes(Rectangle r, int bpp)
{
	return ( (r.max.y-r.min.y) * (r.max.x - r.min.x) * bpp ) >> 3;
}

/* copy_image_part()
 *
 * 	Copy data from one memory buffer to another, but copy
 *	only data within the specified Rectangle
 * Params:
 *	dst	Memory buffer to copy to.
 *	src	First byte of the image in the rectangle to copy from.
 *	r	Rectangle of the area to copy.
 *	width	The full width of the window/buffer in pixels.
 *	bpp	Bitplanes (giving bytes per pixel) 
 */

void
copy_image_part(unsigned char *dst, unsigned char *src, Rectangle r,
		int width, int bpp)
{
	int rectxbytes;		/* bytes per line of the 'r' */
	int dstbytes;		/* bytes per line in the whole window */
	int rectheight;		/* 'r' height */
	int y;

	rectxbytes = (r.max.x - r.min.x) * bpp>>3;
	rectheight = r.max.y - r.min.y;
	dstbytes = width * bpp>>3;

	for(y = 0; y < rectheight; y++) {
		memcpy(dst, src, rectxbytes);
		src += dstbytes;
		dst += rectxbytes;
	}
}

/* redraw_srvimage()
 *
 *	Redraws the image'srvimage' onto the screen image using
 *	libdraw. Both images reside on the display server.
 */

static void
redraw_srvimage(drawstate_t *drawstate)
{
	draw(screen, screen->r, drawstate->srvimage, nil, ZP);
	flushimage(display, 1);	

}

/* update_and_redraw_srvimage()
 *
 *	Updates the internal server image using the data in the
 *	local buffer (that the NSFB library writes to). Also
 *	forces a redraw of the server image.
 */

static int
update_and_redraw_srvimage(drawstate_t *drawstate, Rectangle r,
			int width, int height, int bpp)
{
	copy_image_part(drawstate->updateimage,
			drawstate->localimage + buffer_offset(r.min, width, bpp),
			r, width, bpp);

	loadimage(drawstate->srvimage, r, drawstate->updateimage,
			rect_bytes(r, bpp));
	
	redraw_srvimage(drawstate);
	return 0;
}

static int
plan9_update(nsfb_t *nsfb, nsfb_bbox_t *box)
{
	drawstate_t *drawstate = nsfb->surface_priv;
	Rectangle r;

	r.min.x = box->x0;
	r.min.y = box->y0;
	r.max.x = box->x1;
	r.max.y = box->y1;

//	fprintf(stderr, "DBG: %4d KB update (%3d,%3d) to (%3d, %3d)\n",
//		(r.max.x-r.min.x)*(r.max.y-r.min.y)*(nsfb->bpp>>3) >> 10,
//		r.min.x, r.min.y, r.max.x, r.max.y);

	update_and_redraw_srvimage(drawstate, r,
			nsfb->width, nsfb->height, nsfb->bpp);
	return 0;
}

const nsfb_surface_rtns_t plan9_rtns = {
    .initialise = plan9_initialise,
    .finalise = plan9_finalise,
    .input = plan9_input,
    .claim = plan9_claim,
    .update = plan9_update,
    .cursor = plan9_cursor,
    .geometry = plan9_set_geometry,
};

NSFB_SURFACE_DEF(plan9, NSFB_SURFACE_PLAN9, &plan9_rtns)
