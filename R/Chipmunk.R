

dynamic_shapes <- c('segment', 'circle', 'box', 'polygon')
static_shapes  <- c('static_segment', 'static_circle', 'static_box', 'static_polygon')
valid_shapes <- c(dynamic_shapes, static_shapes)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Chipmunk class
#'
#' @import R6
#'
#' @import chipmunkcore
#' @export
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Chipmunk <- R6::R6Class(
  "Chipmunk",

  public = list(

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @description Initialize the simulation space
    #'
    #' @param gravity vector object indication direction and strength
    #'        of gravity. Default: \code{cpv(0, -100)}
    #' @param time_step simulation step size. smaller is more accurate.
    #'        Default: 0.01
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    initialize = function(gravity = cpv(0, -100), time_step = 0.01) {

      private$time_step = time_step
      private$space <- cpSpaceNew()
      cpSpaceSetGravity(private$space, gravity)


      private$circle_radii <- numeric(0)


      private$poly_count <- 0L

      invisible(self)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Add a shape of the given type
    #'
    #' @param type one of segment, circle, box, polygon and static variants
    #' @param shape the cmShape ExternalPointer
    #' @param friction friction along segment. range [0, 1]
    #' @param elasticity Range [0, 1]
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    add_shape = function(type, shape, friction, elasticity) {
      stopifnot(type %in% c(valid_shapes))

      cpShapeSetFriction(shape, friction)
      cpShapeSetElasticity(shape, elasticity)
      cpSpaceAddShape(private$space, shape)

      private$shape[[type]] <- append(private$shape[[type]], shape)

      invisible(self)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Add a body of the given type
    #'
    #' @param type character name of type. must be a dynamic type
    #' @param body body to add. type = cpBody
    #' @param x,y initial body location
    #' @param vx,vy initial body velocity
    #' @param angle orientation angle in degrees
    #' @param angular_velocity default: 0  degrees/second
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    add_body = function(type, body, x, y, vx, vy, angle, angular_velocity) {
      stopifnot(type %in% c(dynamic_shapes))

      cpBodySetPosition(body, cpv(x, y))
      cpBodySetVelocity(body, cpv(vx, vy))
      cpBodySetAngle   (body, angle * pi/180)
      cpBodySetAngularVelocity(body, angular_velocity * pi/180)
      cpSpaceAddBody(private$space, body)

      private$body[[type]] <- append(private$body[[type]], body)

      invisible(self)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Add a static segment
    #'
    #' @param x1,y1,x2,y2 segment end points
    #' @param friction friction along segment. default 1. range [0, 1]
    #' @param elasticity default: 0 (no bounce). Range [0, 1]
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    add_static_segment = function(x1, y1, x2, y2, friction = 1, elasticity = 0) {

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Static shapes are added to the space static body
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      body <- cpSpaceGetStaticBody(private$space)

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Create a shape
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      shape <- cpSegmentShapeNew(body, cpv(x1, y1), cpv(x2, y2), 0)
      self$add_shape('static_segment', shape, friction = friction, elasticity = elasticity)

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Store segment information
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      this_info <- data.frame(x1, y1, x2, y2)

      private$df[['static_segment']] <- rbind(private$df[['static_segment']], this_info)

      invisible(self)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @description Get the data.frame of all current segments
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_static_segments = function() {
      private$df[['static_segment']]
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Add a circular body to the space
    #'
    #' @param x,y initial body location
    #' @param vx,vy initial body velocity
    #' @param radius radius of body. default: 1
    #' @param mass mass of body. default: 1
    #' @param angular_velocity default: 0  degrees/second
    #' @param friction default: 0.7
    #' @param elasticity default: 0  (no bounce).  Valid range [0, 1]
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    add_circle = function(x, y, vx = 0, vy = 0, radius = 1,
                          angular_velocity = 0,
                          mass = 1, friction = 0.7, elasticity = 0) {

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Create a body and add it to the space
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      moment <- cpMomentForCircle(mass, 0, radius, cpv(0, 0))
      body   <- cpBodyNew(mass, moment);

      self$add_body('circle', body, x = x, y = y, vx = vx, vy = vy,
                    angle = 0,
                    angular_velocity = angular_velocity)

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Create shape and add to space
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      shape <- cpCircleShapeNew(body, radius, cpv(0, 0));
      self$add_shape('circle', shape, friction = friction, elasticity = elasticity);

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Store meta info about shape
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      private$circle_radii <- c(private$circle_radii, radius)


      invisible(self)
    },



    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @description Get the location of all the circles
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_circles = function() {

      bodies <- private$body[['circle']]

      xs <- numeric(length(bodies))
      ys <- numeric(length(bodies))
      for (i in seq_along(bodies)) {
        body  <- bodies[[i]]
        pos   <- cpBodyGetPosition(body)
        pos   <- as.list(pos)
        xs[i] <- pos$x
        ys[i] <- pos$y
      }

      data.frame(idx = seq_along(xs), x = xs, y = ys, r = private$circle_radii)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Add a box body to the space
    #'
    #' @param x,y initial body location
    #' @param vx,vy initial body velocity
    #' @param width,height body width and height
    #' @param angle rotation angle in degrees. default 0
    #' @param radius radius of rounded corner
    #' @param angular_velocity default: 0  degrees/second
    #' @param mass mass of body. default: 1
    #' @param friction default: 0.7
    #' @param elasticity default: 0  (no bounce).  Valid range [0, 1]
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    add_box = function(x, y, vx = 0, vy = 0, width = 1.73, height = 1, angle = 0,
                          radius = 0.05, angular_velocity = 0,
                          mass = 1, friction = 0.7, elasticity = 0) {

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Create a body and add it to the space
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      moment <- cpMomentForBox(mass, width, height)
      body   <- cpBodyNew(mass, moment);

      self$add_body('box', body, x = x, y = y, vx = vx, vy = vy,
                    angle = angle,
                    angular_velocity = angular_velocity)

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Create shape and add to space
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      shape <- cpBoxShapeNew(body, width, height, radius)
      self$add_shape('box', shape, friction = friction, elasticity = elasticity);

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Store meta info about shape
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      private$box_widths  <- c(private$box_widths , width )
      private$box_heights <- c(private$box_heights, height)


      invisible(self)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @description Get the location of all the boxes
    #'
    #' @return a data.frame with idx, width, height and rotation angle (radians)
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_boxes = function() {

      bodies <- private$body[['box']]

      xs <- numeric(length(bodies))
      ys <- numeric(length(bodies))
      rs <- numeric(length(bodies))

      for (i in seq_along(bodies)) {
        body  <- bodies[[i]]
        pos   <- cpBodyGetPosition(body)
        pos   <- as.list(pos)
        xs[i] <- pos$x
        ys[i] <- pos$y
        rs[i] <- cpBodyGetAngle(body)
      }

      data.frame(
        idx    = seq_along(xs),
        x      = xs,
        y      = ys,
        angle  = rs,
        width  = private$box_widths,
        height = private$box_heights
      )
    },



    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @description Get the location of all the boxes as a list of corner vertices
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_boxes_as_polygons = function() {

      box_centroids <- cm$get_boxes()

      box_centroids$xc <- box_centroids$x
      box_centroids$yc <- box_centroids$y

      box_vertices <- rbind(
        transform(box_centroids, vertex = 1, x = -width/2, y = -height/2),
        transform(box_centroids, vertex = 2, x =  width/2, y = -height/2),
        transform(box_centroids, vertex = 3, x =  width/2, y =  height/2),
        transform(box_centroids, vertex = 4, x = -width/2, y =  height/2)
      )

      box_vertices <- box_vertices[, c('idx', 'vertex', 'xc', 'yc', 'x', 'y', 'angle')]
      box_vertices <- box_vertices[with(box_vertices,order(idx, vertex)),]

      polys <- transform(
        box_vertices,
        x1 = x * cos(angle) - y * sin(angle) + xc,
        y  = x * sin(angle) + y * cos(angle) + yc
      )

      polys$x <- polys$x1

      polys <- polys[, c('idx', 'vertex', 'x', 'y')]

      polys
    },



    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Add a convex hull polygon to the space
    #'
    #' @param xs,ys polygon vertices
    #' @param x,y initial body location
    #' @param angle initial rotation angle in degrees. default 0
    #' @param vx,vy initial body velocity
    #' @param radius radius of rounded corners
    #' @param angular_velocity default: 0  degrees/second
    #' @param mass mass of body. default: 1
    #' @param friction default: 0.7
    #' @param elasticity default: 0  (no bounce).  Valid range [0, 1]
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    add_polygon = function(
      xs, ys,
      angle = 0,
      x = 0, y = 0,
      vx = 0, vy = 0,
      radius = 0.05,
      angular_velocity = 0,
      mass = 1, friction = 0.7, elasticity = 0) {

      if (length(xs) != length(ys) || length(xs) < 3) {
        stop("add_polygon requires at least vertices")
      }

      # The vertices as a list of 'cpVect' objects
      verts <- cpVect(xs, ys)


      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Create a body and add it to the space
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      moment <- cpMomentForPoly(
        m      = mass,
        count  = length(xs),
        verts  = verts,
        offset = cpVect(0, 0),
        radius = radius
      )

      body <- cpBodyNew(mass, moment);

      self$add_body('polygon', body, x = x, y = y, vx = vx, vy = vy,
                    angle = angle,
                    angular_velocity = angular_velocity)

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Create shape and add to space
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      shape <- cpPolyShapeNew(
        body      = body,
        count     = length(xs),
        verts     = verts,
        transform = cpTransformIdentity(),
        radius    = radius
      )
      self$add_shape('polygon', shape, friction = friction, elasticity = elasticity);


      private$poly_count <- private$poly_count + 1L

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # Store meta info about shape
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # inefficent accumulation of polygons. fixme.
      private$poly_verts <- rbind(
        private$poly_verts,
        data.frame(
          x      = xs,
          y      = ys,
          idx    = private$poly_count,
          vertex = seq_along(xs)
        )
      )

      invisible(self)
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @description Get the location of all the polygons
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_polygons = function() {

      bodies <- private$body[['polygon']]

      xs <- numeric(length(bodies))
      ys <- numeric(length(bodies))
      rs <- numeric(length(bodies))

      for (i in seq_along(bodies)) {
        body  <- bodies[[i]]
        pos   <- cpBodyGetPosition(body)
        pos   <- as.list(pos)
        xs[i] <- pos$x
        ys[i] <- pos$y
        rs[i] <- cpBodyGetAngle(body)
      }

      poly_centroids <- data.frame(
        idx    = seq_along(xs),
        xc     = xs,
        yc     = ys,
        angle  = rs
      )

      polys <- merge(poly_centroids, private$poly_verts)

      polys <- transform(
        polys,
        x1 = x * cos(angle) - y * sin(angle) + xc,
        y  = x * sin(angle) + y * cos(angle) + yc
      )

      polys$x <- polys$x1

      polys <- polys[, c('idx', 'vertex', 'x', 'y')]

      polys
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' @description Advanced: Get the list of Chipmunk \code{cpBody} objects
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_bodies = function() {
      private$body
    },


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #' Advance the simulation by N timesteps
    #'
    #' @param N number of time steps to advance
    #'
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    advance = function(N = 1) {

      for (i in seq_len(N)) {
        cpSpaceStep(private$space, private$time_step)
      }

      invisible(self)
    }

  ),

  private = list(
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # @field space the chipmunk simulation space.
    # @field time_step simulation time_step
    # @field body named list of lists of all bodies by type
    # @field shape named list of lists of all shapes by type
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    space          = NULL,
    time_step      = NULL,

    body           = list(),
    shape          = list(),
    df             = list(),


    circle_radii   = NULL,

    box_widths     = NULL,
    box_heights    = NULL,

    poly_verts     = NULL,
    poly_count     = NULL,

    static_segments    = NULL
  )
)






















