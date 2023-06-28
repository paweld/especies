<!-- Form validation -->
function validateTextSearch()
{
  if (document.mainform.name.value == "")
  {
    alert("Please enter search terms into the text box.");
    document.mainform.name.focus();
    return false;
  } 
  else if (document.mainform.name.value.split(" ").length != 2)
  {
    alert("Please enter a binomial specific epithet into the text box.\n"+
     "Remember: Only species are true natural entities!");
    document.mainform.name.focus();
    return false;
    
  }
  else 
  {
  	var taxon_pattern = /^\s*[A-Za-z]+(\s+[A-Za-z]+)*\s*$/;
   	if (taxon_pattern.test(document.mainform.name.value))
  	{
	  	var result = true;
   	 	return result;
   	 }
   	 else
   	 {
		alert("Not a valid entry!");
		document.mainform.name.focus();
		return false;
   	 }
  }
}
