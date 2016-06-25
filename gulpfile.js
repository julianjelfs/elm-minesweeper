var gulp = require('gulp');
var elm  = require('gulp-elm');
var plumber = require('gulp-plumber');
var livereload = require('gulp-livereload');

gulp.task('elm-init', elm.init);

gulp.task('elm', ['elm-init'], function(){
    return gulp.src('src/*.elm')
        .pipe(plumber())
        .pipe(elm())
        .pipe(gulp.dest('dist'))
        .pipe(livereload());
});

gulp.task('css', function(){
    return gulp.src('dist/*.css')
        .pipe(gulp.dest(''))
        .pipe(livereload());
});

gulp.task('default',['watch']);

gulp.task('watch', function(){
    livereload.listen();
    gulp.watch('src/*.elm',['elm']);
    gulp.watch('dist/*.css', ['css']);
});
