/* libnsfb ploygon plotter test program */

#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <math.h>

#include "libnsfb.h"
#include "libnsfb_plot.h"
#include "libnsfb_event.h"

#define UNUSED(x) ((x) = (x))

extern void ram_register_surface(void);
extern void plan9_register_surface(void);

void
init_libs(void)
{
	ram_register_surface();
	plan9_register_surface();	
}

int main(int argc, char **argv)
{
    const char *fename;
    enum nsfb_type_e fetype;
    nsfb_t *nsfb;
    nsfb_event_t event;
    int waitloop = 3;

    init_libs();

    nsfb_bbox_t box;
    uint8_t *fbptr;
    int fbstride;

    int sides;
    int radius;
    nsfb_point_t *points;
    int loop;
//    nsfb_plot_pen_t pen;

    if (argc < 2) {
        fename="plan9";
    } else {
        fename = argv[1];
    }

    fetype = nsfb_type_from_name(fename);
    if (fetype == NSFB_SURFACE_NONE) {
        fprintf(stderr, "Unable to convert \"%s\" to nsfb surface type\n", fename);
        return 1;
    }

    nsfb = nsfb_new(fetype);
    if (nsfb == NULL) {
        fprintf(stderr, "Unable to allocate \"%s\" nsfb surface\n", fename);
        return 2;
    }

    if (nsfb_init(nsfb) == -1) {
        fprintf(stderr, "Unable to initialise nsfb surface\n");
        nsfb_free(nsfb);
        return 4;
    }

    /* get the geometry of the whole screen */
    box.x0 = box.y0 = 0;
    nsfb_get_geometry(nsfb, &box.x1, &box.y1, NULL);
    if ((box.x1 == 0) || (box.y1 == 0)) {
        /* if surface was created with no size set a default */
        nsfb_set_geometry(nsfb, 800, 600, NSFB_FMT_ANY);
        nsfb_get_geometry(nsfb, &box.x1, &box.y1, NULL);
    }

/*    fprintf(stderr, "DBG: nsfb_get_geometry() retured h=%d w=%d\n",
		box.x1, box.y1); */ /*returned h=800, w=600 */


/*    fprintf(stderr, "DBG: Calling nsfb_get_buffer(fbstride = %d)\n",
		fbstride); */
    nsfb_get_buffer(nsfb, &fbptr, &fbstride);


    /* claim the whole screen for update */
    //fprintf(stderr, "DBG: Calling nsfb_claim()...\n");
    nsfb_claim(nsfb, &box);

    //fprintf(stderr, "DBG: Calling plot_clg()...\n");
    nsfb_plot_clg(nsfb, 0xffffffff);

	{
		nsfb_bbox_t bx;

		bx.x0=bx.y0 = 100;	/* black */
		bx.x1=200; bx.y1=200;
		nsfb_plot_rectangle_fill(nsfb, &bx, 0xff000000);

		bx.x0=300;  	/* red */
		bx.x1=400; 
		nsfb_plot_rectangle_fill(nsfb, &bx, 0xff0000ff);

		bx.x0=500;		/* green */
		bx.x1=600;
		nsfb_plot_rectangle_fill(nsfb, &bx, 0xff00ff00);

		bx.x0=700;	/* blue */
		bx.x1=800;
		nsfb_plot_rectangle_fill(nsfb, &bx, 0xffff0000);
	}





/* jam test: draw diagonal extra line (100,100) to (500,500) */
	{	/* new scope */
		nsfb_plot_pen_t pn;
		nsfb_bbox_t bx;
		bx.x0 = bx.y0 = 0;
		bx.x1 = bx.y1 = 500;
//		pn.stroke_colour = 0xff000000; /* should be black */
//		pn.stroke_colour = 0x00ff0000; /* is red */
//		pn.stroke_colour = 0x00ff00ff; /* is violet (R+B) */
//		pn.stroke_colour = 0xffff0000; /* is red */
		pn.stroke_colour = 0xff00ff00; /* is white */
		pn.stroke_colour = 0xff0000ff; /* is white */
		pn.stroke_colour = 0xff00ffff; /* is white */
		pn.stroke_colour = 0x000000ff; /* is while*/
		pn.stroke_colour = 0x00800000; /* is light red*/
		pn.stroke_colour = 0xff000000; /* is white */
		pn.stroke_colour = 0xffffff00; /* is yellow? */
		pn.stroke_colour = 0xffffffff; /* is yellow? */
		pn.stroke_colour = 0xff000000; /* should be black */
		


		nsfb_plot_line(nsfb, &bx, &pn);
	}

//  fprintf(stderr, "DBG: Calling nsfb_update()...\n");
    nsfb_update(nsfb, &box);
    
    /* wait for quit event or timeout */
    while (waitloop > 0) {
	if (nsfb_event(nsfb, &event, 8000)  == false) {
	    break;
	}
	if (event.type == NSFB_EVENT_CONTROL) {
	    if (event.value.controlcode == NSFB_CONTROL_TIMEOUT) {
		/* timeout */
		waitloop--;
	    } else if (event.value.controlcode == NSFB_CONTROL_QUIT) {
		break;
	    }
	}
    }

    nsfb_free(nsfb);

    return 0;
}

/*
 * Local variables:
 *  c-basic-offset: 4
 *  tab-width: 8
 * End:
 */
