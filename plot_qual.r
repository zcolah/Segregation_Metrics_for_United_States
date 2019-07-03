plot.qual<-function(x,x.locs=c(0.01,0.99),y.locs=c(0,1),steps=NULL,sp.names=NULL,dt.tx=T,rsc=T,
	ln.st=NULL,rs.ln=c(3,15),ln.cl='RdYlGn',alpha=0.7,leg=T,rnks=FALSE,...){

	require(RColorBrewer)
	require(scales)
	
	if(length(x.locs) != 2 | length(y.locs) != 2) 
		stop('x and y dimensions must be two-element vectors')
	
	if(x.locs[1]<0 | x.locs[2]>1 | y.locs[1]<0 | y.locs[2]>1) 
		stop('x and y dimensions must in range of 0--1')
 
	dim.x<-c(0,1) #plot dims x
	dim.y<-c(0,1) #plot dims y
	wrn.val<-F
 
	x[,1]<-as.character(x[,1]) 
	tot.sp<-ncol(x)-1
	sp.col<-2:ncol(x)
	
	#rescale if T, sort legend for later
	sp.orig<-x[,sp.col]
	if(length(rs.ln)==1) rsc<-F
	if(rsc) x[,sp.col]<-rescale(as.matrix(x[,sp.col]),rs.ln)
	if(rsc==F & leg) leg<-F #no legend if line widths aren't rescaled
	
	#reorder species columns, add rank as integer
	first.ord<-order(x[1,sp.col],decreasing=T)
	x[,sp.col]<-x[,sp.col][,first.ord]
	names(x)[sp.col]<-names(x)[sp.col][first.ord]
	names(x)[sp.col]<-paste(1:tot.sp,names(x)[sp.col],sep='. ')
	
	#list of spp by date, arranged in decreasing order for each date
	dt.dat.srt<-vector('list',nrow(x))
	names(dt.dat.srt)<-x[,1]
	for(val in 1:nrow(x)){
		tmp<-t(x[val,sp.col])
		tmp<-tmp[order(tmp,decreasing=T),,drop=F]
		dt.dat.srt[[val]]<-tmp
		}
	
	#initiate plot object
 	plot(dim.x,dim.y,type='n',axes=F,xlab='',ylab='',...) 
	
	#subset for steps, if provided
	if(!is.null(steps)) dt.dat.srt<-dt.dat.srt[names(dt.dat.srt) %in% steps]
	
	#plot legend
	if(leg){
		y.locs[1]<-0.05*diff(y.locs)+y.locs[1]
		leg.txt<-format(round(seq(min(sp.orig),max(sp.orig),length=5),2),nsmall=2,digits=2)
		leg.wds<-seq(rs.ln[1],rs.ln[2],length=5)
		legend('bottom',(y.locs[1]-y.olds)/2,col=alpha('black',alpha),lwd=leg.wds,legend=leg.txt,bty='n',
			horiz=T)
		}	
	
	#x locations
	x.vals<-rep(seq(x.locs[1],x.locs[2],length=length(dt.dat.srt)),each=tot.sp)
	x.vals<-split(x.vals,x.vals)
	
	#y locations, rearranged in loop, exception if dates are plotted
	if(dt.tx) y.vals<-rev(seq(y.locs[1],y.locs[2],length=tot.sp+1))[-1]
	else y.vals<-rev(seq(y.locs[1],y.locs[2],length=tot.sp))
	
	#get line colors
	if(length(ln.cl)==1)
		if(ln.cl %in% row.names(brewer.pal.info)){
			pal.num<-brewer.pal.info[row.names(brewer.pal.info) == ln.cl,1]
			ln.cl<-brewer.pal(pal.num,ln.cl)
			}
	line.cols<-alpha(colorRampPalette(ln.cl)(tot.sp),alpha)
	
	#define distance of lines from labels
	if(is.null(ln.st)){
		str.max<-max(strwidth(row.names(dt.dat.srt[[1]])))
		if(diff(x.locs)-length(dt.dat.srt)*str.max < 0){
			warning('not enough space for lines between columns')
			wrn.val<-T
			}
		else
			ln.st<-0.2*str.max + str.max/2
		}
	
	for(val in 1:(length(dt.dat.srt)-1)){
		
		#temp data to plot
		plt.tmp<-dt.dat.srt[c(val,val+1)]
		x.tmp<-x.vals[c(val,val+1)]
	
		#plot temp text for column, remove spp if rnks
		rowtxt <- row.names(plt.tmp[[1]])
    if(rnks)
	    rowtxt <- gsub('([1-9]*)\\..*', '\\1', rowtxt) 
		text(x.tmp[[1]],y.vals,rowtxt)
 
		if(val == length(dt.dat.srt)-1){
		  rowtxt <- row.names(plt.tmp[[2]])
		  if(rnks)
	      rowtxt <- gsub('([1-9]*)\\..*', '\\1', rowtxt) 
			text(x.tmp[[2]],y.vals,rowtxt)
			if(dt.tx){
				dt.txt<-substitute(italic(x),list(x=names(plt.tmp)[2]))
				text(unique(x.tmp[[2]]),y.locs[2],dt.txt)
				}
			}	
		
		if(dt.tx){
			dt.txt<-substitute(italic(x),list(x=names(plt.tmp)[1]))
			text(unique(x.tmp[[1]]),y.locs[2],dt.txt)
			}
		
		srt.ln.y<-match(row.names(plt.tmp[[1]]),row.names(plt.tmp[[2]]))
		
		#if no line rescale, use first element of rs.ln
		if(rsc) lwd.val<-plt.tmp[[1]][,1]
		else lwd.val<-rep(rs.ln[1],tot.sp)
		
		#vector for species selection of line segments
		if(is.null(sp.names)) sel.sp<-rep(T,tot.sp)
		else{
			sel.names<-unlist(lapply(strsplit(row.names(plt.tmp[[1]]),' '),function(x) x[2]))
			sel.sp<-(sel.names %in% sp.names)
			}
			
		#for lines
		if(!wrn.val)
			segments(
				x.tmp[[1]][sel.sp]+ln.st,
				y.vals[sel.sp],
				x.tmp[[2]][sel.sp]-ln.st,
				y.vals[srt.ln.y][sel.sp],
				col=line.cols[sel.sp],
				lwd=lwd.val[sel.sp]
				)
		
		#resort color vector for next colummn
		srt.cl.y<-match(row.names(plt.tmp[[2]]),row.names(plt.tmp[[1]]))
		line.cols<-line.cols[srt.cl.y]
		
		}
	
	}