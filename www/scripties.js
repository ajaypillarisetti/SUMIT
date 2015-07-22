$(document).ready(function() {
	$("ul.treeview-menu li").click(function(){
		$('table.table.table-striped.table-hover.dataTable.no-footer').delay(1000).attr('style', function(i, style)
			{return style.replace(/width[^;]+;?/g, '');
		});
	});
});    
