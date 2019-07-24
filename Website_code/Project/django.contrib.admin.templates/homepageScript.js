//url request of msadetails
function onSelectChange(val){
      console.log(val);     
     // console.log('selected event', val.target.selectedOptions[0].innerText);
  //    params = val.target.selectedOptions[0].innerText;
  localStorage.setItem('city',val);
      var http = new XMLHttpRequest();
  var url = 'http://45.35.64.128:8000/msadetails?MSA='+ val;
//  var data = params ;
  http.open('GET', url, true);
  http.withCredentials = true;
http.setRequestHeader("Content-Type", "application/json;charset=UTF-8");
  //Send the proper header information along with the request
//http.setRequestHeader('content-type', 'application/json');
//http.setRequestHeader('content-type', 'json');
//http.setRequestHeader('Content-type','text/html');
 // http.setRequestHeader('Content-type', 'application/x-www-form-urlencoded');
http.setRequestHeader("Access-Control-Allow-Origin","*");

http.setRequestHeader("Access-Control-Allow-Credentials", "true");
http.setRequestHeader("Access-Control-Allow-Methods", "GET,HEAD,OPTIONS,POST,PUT");
http.setRequestHeader("Access-Control-Allow-Headers", "Access-Control-Allow-Headers, Origin,Accept, X-Requested-With, Content-Type, Access-Control-Request-Method, Access-Control-Request-Headers");


  http.onreadystatechange = function() {//Call a function when the state changes.
      if(http.readyState == 4 && http.status == 200) {
        //alert(http.responseText);
      window.location="details.html?city="+val;

      }
  }
  http.send(val)
}
//Button navigations
  function openCity(url, Top10) {
    window.location = url;
    // var i, tabcontent, tablinks;
    // tabcontent = document.getElementsByClassName("tabcontent");
    // for (i = 0; i < tabcontent.length; i++) {
    //   tabcontent[i].style.display = "none";
    // }
    // tablinks = document.getElementsByClassName("tablinks");
    // for (i = 0; i < tablinks.length; i++) {
    //   tablinks[i].className = tablinks[i].className.replace(" active", "");
    // }
    // document.getElementById(cityName).style.display = "block";
    // evt.currentTarget.className += " active";
  }
  
   <!-- function onSelect(A,B,C,D){ -->
		<!-- console.log(A); -->
		<!-- console.log(B); -->
		<!-- console.log(C); -->
		<!-- console.log(D); -->
		<!-- console.log('selected event', val.target.selectedOptions[0].innerText); -->
		<!-- params = val.target.selectedOptions[0].innerText; -->
		<!-- localStorage.setItem('city',A); -->
		<!-- localStorage.setItem('Age',B); -->
		<!-- localStorage.setItem('Tax',C); -->
		<!-- localStorage.setItem('GDP',D); -->
		<!-- var http = new XMLHttpRequest(); -->
		<!-- var url = 'http://45.35.64.128:8000/kmeansmodel?MSA=&AGE2034=&AvgPropTaxPerCap=&GDPperCap='+A+B+C+D; -->
		<!-- var data = params ; -->
		<!-- http.open('GET', url, true); -->

		<!-- Send the proper header information along with the request -->
		<!-- http.setRequestHeader('Content-type', 'application/x-www-form-urlencoded'); -->

		<!-- http.onreadystatechange = function() { -->
		<!-- Call a function when the state changes. -->
		<!-- if(http.readyState == 4 && http.status == 200) { -->
		<!-- alert(http.responseText); -->
		     <!-- window.location="C:/CityProject/templates/details.html" ; -->
		<!-- window.location="details.html" ; -->

      <!-- } -->
  <!-- } -->
  <!-- http.send(A,B,C,D) -->
 <!-- } -->
 //Plotly Map 
Plotly.d3.csv('https://raw.githubusercontent.com/abachoudhary/cityamericadata/master/data/Map_Msadata.csv', function(err, rows){
  
  var classArray = unpack(rows, 'class');

  var msa = unpack(rows, 'name'); 
var classes = [...new Set(classArray)];
   
  function unpack(rows, key) {
    return rows.map(function(row) { return row[key]; });
  }
 
  var data =[{type: 'scattermapbox',
       lat: unpack(rows, 'reclat'),
       lon: unpack(rows, 'reclong'),
       mode:'markers',
       marker: {  color: 'rgb(255, 127, 14)',size:10 },
       text: msa,
      // hoverinfo: 'Population'
   }]

  var layout = {
     title: 'MSA Landing Locations',
     
     font: {
         color: 'white'
     },
    dragmode: 'zoom', 
    
    mapbox: {
      center: {
        lat: 38.03697222, 
        lon: -90.70916722
      }, 
      domain: {
        x: [0, 1], 
        y: [0, 1]
      }, 
      style: 'dark', 
      zoom: 3
    }, 
    margin: {
      r: 20, 
      t: 40, 
      b: 20, 
      l: 20, 
      pad: 0
    }, 
    paper_bgcolor: '#191A1A', 
    plot_bgcolor: '#191A1A', 
    showlegend: true,
     annotations: [{
         x: 0,
       y: 0,
       xref: 'paper',
       yref: 'paper',
            text: 'Source: <a href="http:45.35.64.128:8000/home" style="color: rgb(255,255,255)">CityAmerica</a>',
         showarrow: false
     }]
  };
  
  Plotly.setPlotConfig({
    mapboxAccessToken: 'pk.eyJ1IjoiZXRwaW5hcmQiLCJhIjoiY2luMHIzdHE0MGFxNXVubTRxczZ2YmUxaCJ9.hwWZful0U2CQxit4ItNsiQ'
  });
  
  Plotly.plot('graphDiv', data, layout, {showSendToCloud: true});
}); 	  
  
//Footer Scrolling
$(document).ready(function(){
  // Initialize Tooltip
  $('[data-toggle="tooltip"]').tooltip(); 
  
  // Add smooth scrolling to all links in navbar + footer link
  $(".navbar a, footer a[href='#myPage']").on('click', function(event) {

    // Make sure this.hash has a value before overriding default behavior
    if (this.hash !== "") {

      // Prevent default anchor click behavior
      event.preventDefault();

      // Store hash
      var hash = this.hash;

      // Using jQuery's animate() method to add smooth page scroll
      // The optional number (900) specifies the number of milliseconds it takes to scroll to the specified area
      $('html, body').animate({
        scrollTop: $(hash).offset().top
      }, 900, function(){
   
        // Add hash (#) to URL when done scrolling (default click behavior)
        window.location.hash = hash;
      });
    } // End if
  });
})
