/*===================================================================================*/
/*	OWL CAROUSEL
/*===================================================================================*/

$(document).ready(function () {
	

	$(".owl-news").owlCarousel({
		autoPlay: 5000,
		slideSpeed: 200,
		paginationSpeed: 600,
		rewindSpeed: 800,
		stopOnHover: true,
		navigation: true,
		pagination: true,
		rewindNav: true,
		singleItem: true,
		autoHeight: true,
		navigationText: ["<i class='icon-left-open-mini'></i>", "<i class='icon-right-open-mini'></i>"]
	});
	
});

/*===================================================================================*/
/*	ISOTOPE BLOG
/*===================================================================================*/


$(document).ready(function () {
	
	var $container = $('.posts');
	
	$container.imagesLoaded(function () {
		$container.isotope({
			itemSelector: '.post'
		});
	});
	
  $('div.navigation a').click(function() {
  	$container.isotope({ filter: '*' });
  })

	$('.format-filter li a, .format-wrapper a').click(function () {
		
		var selector = $(this).attr('data-filter');
		
		$container.isotope({
			filter: selector
		});
		
		$('.format-filter li a').removeClass('active');
		$('.format-filter li a[data-filter="'+selector+'"]').addClass('active');
		
		$('html, body').animate({
			scrollTop: $('.format-filter').offset().top -130
		}, 600);
		
		return false;
		
	});
	
	$(window).on('resize', function () {
		$('.posts').isotope('reLayout')
	});
	
});
