space-invaders:
	raco exe --gui --orig-exe space-invaders.rkt

tar: 
	tar -C . -czvf space-invaders.tar.gz space-invaders assets
