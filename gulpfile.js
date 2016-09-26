var gulp = require('gulp')
	, pug = require('gulp-pug')
	, reveal = require('gulp-revealjs')
	;

gulp.task('default', function() {
	gulp.start('compile', 'images');
});

gulp.task('images', () => gulp.src('sources/img/**')
	.pipe(gulp.dest('lections/img'))
);

gulp.task('compile', () => gulp.src('sources/*.pug')
	.pipe(pug({}))
	.pipe(reveal())
	.pipe(gulp.dest('lections/'))
);
