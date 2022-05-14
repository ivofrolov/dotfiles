function pyclean --description 'Delete Python bytecode'
	find $argv \( -name \*.pyc -or -name \*.pyo -or -name __pycache__ \) -delete
end
