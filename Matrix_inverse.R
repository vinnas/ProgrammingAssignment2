##function to create special matrix

makeCatcheMatrix<-function(x=matrix(1:4,2,2))
{
	##checking whether the matrix is inversable.
	if(class(try(solve(x)))=="try-error")
	{
		message("The matrix cannot be inversed");
		return();
	}
	m<-NULL;
	set<-function(y)
	{
		x<<-y;
		m<<-NULL;
	}
	get<-function()x;
	setinv<-function(inv)m<<-inv;
	getinv<-function()m;
	list(set=set,get=get,setinv=setinv,getinv=getinv);	
}




##function to get the inverse of matrix
catchesolve<-function(x)
{
	m<-x$getinv();
	##checking whether the matrix is already catched
	if(!is.null(m))
	{
	 message("Getting catched data");
	 return(m);
	}
	data<-x$get();
	m<-solve(data);#inversing a matrix
	x$setinv(m);
	return(m);
}
