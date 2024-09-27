const gulp = require("gulp");
const elm = require("gulp-elm");
const plumber = require("gulp-plumber");
const livereload = require("gulp-livereload");
const browserSync = require("browser-sync").create();

// Elm task
function elmTask() {
  return gulp
    .src("src/*.elm")
    .pipe(plumber())
    .pipe(elm({ debug: false, optimize: true }))
    .pipe(gulp.dest("dist"))
    .pipe(livereload())
    .pipe(browserSync.stream());
}

// CSS task
function cssTask() {
  return gulp
    .src("dist/*.css")
    .pipe(gulp.dest("dist"))
    .pipe(livereload())
    .pipe(browserSync.stream());
}

// Start the development server
function serve() {
  browserSync.init({
    server: {
      baseDir: "dist", // Serve files from the 'dist' directory
    },
  });

  // Watch for changes
  gulp.watch("src/*.elm", elmTask);
  gulp.watch("dist/*.css", cssTask);
}

// Define default task
exports.default = gulp.series(gulp.parallel(elmTask, cssTask), serve);

// Export individual tasks
exports.elm = elmTask;
exports.css = cssTask;
