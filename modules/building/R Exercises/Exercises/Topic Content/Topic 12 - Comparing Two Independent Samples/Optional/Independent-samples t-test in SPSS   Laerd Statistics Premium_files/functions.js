$(function () {
	
		$("h6").append('<span class="top"><a href="#top">top ^</a></span>');
		
	//	$(".spss-proc li").after('<span class="litop"><a href="#top">top ^</a></span>');
		$(".statements p").after('<div class="quill"></div>');
	
		$("a[href='#top']").click(function() {
		$("html, body").animate({ scrollTop: 0 }, "slow");
		return false;
		});
		
		//initial setup
		$('p.gen-content').show();
		$('p.apa-content').hide();
	
		//when a tab is clicked
		$('.statement li').click(function() {
			//get class of clicked tab
			var class_of_tab = $(this).attr('class').split(' ')[0];
			//remove active class from all statement
			$('.statement li').removeClass('active');
			//add active class to the clicked tab's class
			$('.statement li.' + class_of_tab).addClass('active');
			//hide all statements 
			$('.statement p').hide();
			//show statement that has been selected 
			$('p.' + class_of_tab + '-content').show();
		});
		
		
		
});