# 任务运行的时长很长的时候，需要使用这个进度条函数来显示执行进度
# Only avaliable for single thread R app
tick.helper <- function(total, step = 5 / 100) {
		
	# 因为R语言的closure里面的变量都是局部变量，所以修改局部变量并不会影响全局的记录进度的变量
	# 所以在这里必须要使用全局变量赋值来实现进度的记录
		
	tick.helper.i  <<- 0;
	tick.helper.p5 <<- total * step;
	tick.helper.progress <<- 0;
	
	return(function() {
		tick.helper.i <<- tick.helper.i + 1;
		
		if (tick.helper.i >= tick.helper.p5) {
			tick.helper.i <<- 1;
			tick.helper.progress <<- tick.helper.progress + step * 100;
			tick.helper.progress <<- round(tick.helper.progress, 0);
			
			cat(tick.helper.progress);
			cat(" ");
		}
	});	
}