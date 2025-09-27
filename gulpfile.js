const gulp = require('gulp');
const pug = require('gulp-pug');
const reveal = require('gulp-revealjs');

// Compile pug to reveal.js HTML
function compile() {
	return gulp.src('sources/*.pug')
		.pipe(pug({}))
		.pipe(reveal({ revealOptions: { mouseWheel: true } }))
		.pipe(gulp.dest('lections/'));
}

// Copy images
function images() {
	return gulp.src('sources/img/**')
		.pipe(gulp.dest('lections/img'));
}

// Default task (runs both compile + images in parallel)
exports.default = gulp.parallel(compile, images);

// Export individual tasks if needed
exports.compile = compile;
exports.images = images;
