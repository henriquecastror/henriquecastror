use "files/CEOSAL1.DTA" , replace
replace salary = salary * 1000
qui reg salary roe 
esttab
