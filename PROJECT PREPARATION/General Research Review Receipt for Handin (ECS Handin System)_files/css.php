@import url( '/style/css/boxes.css' );
@import url( '/style/css/header.css' );
@import url( '/style/css/sidebar.css' );
@import url( '/style/css/intramenu.css' );
/* Dynamic CSS Style Sheet */
/* Your browser identified as: GECKO */

/****************************************
 * general top level bits
 ****************************************/


body {
	color: black;
	margin: 0px 0px 0px 0px;
	padding: 0px 0px 0px 0px;
}


/****************************************
 * microformat bits
 ****************************************/
abbr.dtstart, abbr.dtend {
	border-bottom: none;
}

/****************************************
 * end microformat bits
 ****************************************/

/****************************************
 * start form library bits
 ****************************************/

div.forms_input_field {
	padding: 4px;
	border: solid 1px #ccc;
	border-width: 1px 1px 1px 10px;
	border-color: #ccc #fff #fff #ccc;
	margin-bottom: 8px;
}
div.forms_input_field textarea { width: 100%; }
/*div.forms_input_field input { width: 100%; }*/

div.forms_input_field p {
	margin: 0 0 0.5em 0;
}

/****************************************
 * end form library bits
 ****************************************/


.noScreen {
	display: none;
}

.hide {
	display: none;
}

#content {
	margin-top: 0px;
	margin-right: 0px;
	margin-left: 0px;
	margin-bottom: 15px;
	border: solid black 0px;
	padding-right: 30px;
	padding-left: 30px;
	padding-bottom: 30px;
	padding-top: 0px;
	text-align: left;
	clear: left;
}

.pageContentBars0 {
}
.pageContentBars1 {
	float: left;
	width: 70%;
}
.pageContentBars2 {
	float: left;
	width: 45%;
}

/****************************************
 * default tabs
 ****************************************/

#xtabs ul {
	margin: 0;
	padding: 10px 10px 0 10px;
	list-style: none;
}

#xtabs li {
	float:left;
	background:url("/style/images/header/tabs/tab_left.png") no-repeat left top;
	margin:0;
	padding:0 0 0 8px;
}

#xtabs a {
	float:left;
	display:block;
	background:url("/style/images/header/tabs/tab_right.png") no-repeat right top;
	padding:12px 15px 4px 6px;
	text-decoration:none;
	font-weight:bold;
	color: white;
}

#xtabs .current a {
	background:url("/style/images/header/tabs/tab_right_on.png") no-repeat right top;
}
#xtabs li.current {
	background:url("/style/images/header/tabs/tab_left_on.png") no-repeat left top;
}




/****************************************
 * sides
 ****************************************/

.rightside {
	float: right;
	clear: right;
	margin-left: 2%;
	width: 25%;
}

.leftside {
	clear: left;
	float: left;
	margin-right: 2%;
	width: 25%;
}

/****************************************
 * seminars
 ****************************************/

dl.seminars dt
{
	margin-top: 0.5em;
	font-size: 120%;
}
dl.seminars dd
{
	margin-left: 0px;
	padding-left: 4em;
	padding-bottom: 0.5em;
	border-bottom: 1px #ccc dashed;
}
	


/****************************************
 * misc
 ****************************************/

.illustration {
	float: right;
	clear: right;
	margin: 0px 0px 1em 1em;
	text-align: right;
}
.illustrationNoClear {
	float: right;
	margin: 0px 0px 1em 1em;
}
.illustration .copyright {
	font-size: 80%;
}

.thumbnail {
	float: left;
	margin: 0px 1em 0.75em 0em;
}
.newsItem {
}

.jumpLinks h2 {
	margin-top: 0px;
}
.jumpLinks img {
	float: left;
	margin-bottom: 10px;
	margin-right: 12px;
}
.jumpLinks .row {
	padding-top: 10px;
	border-top: 1px #CCCCCC solid;
}
.jumpLinks p {
	margin: 0px;
	padding: 0px;
	font-size: 0.9em;
}

.clearBoth {
	clear: both;
}

.citation {
	margin-bottom: 1em;
}
/*
.citation a {
	text-decoration: none;
}
.citation a:hover {
	text-decoration: underline;
	color: black;
}
*/


body {
	background: #ffffff;
	font-family: Verdana, Arial, Helvetica, sans-serif;
	font-size: 76%;
	margin: 0 auto;
	padding: 0px;
}
a {
	color: #000000;
}
a:hover {
	color: #666666;
}
h1 {
	font-size: 1.7em;
	margin-top: 0px;
}
h1.withIntro {
	margin-bottom: 0px;
}

h2 {
	font-size: 1.4em;
/*	margin: 0px; */
}
h2.current {
	font-size: 1.7em;
}
h3 {
	font-size: 1.1em;
	/*margin: 0px;*/
}
hr {
	clear: both;
	display: block;
	height: 1px;
	border-top: 1px solid #CCCCCC;
	border-left: 0 none;
	border-right: 0 none;
	border-bottom: 0 none;
	margin: 1em 0;
	padding: 0;
}
form {
	margin: 0px;
}
.hide {
	display: none;
}

#content {
	padding: 0px;
	margin: 16px 16px 0px 16px;
	text-align: left;
}

#contentMain {
	float: left;
	width: 69%;
	clear: left;
}
#contentMain.invertLayout {
	float: right;
}
p.intro {
	font-size: 0.9em;
	font-weight: bold;
	margin-top: 0px;
	margin-bottom: 14px;
}


.headlineBoxContent h3 {
	margin: 0px 0px 14px;
	font-size: 1em;
}
.headlineBoxContent h4 {
	font-size: 0.9em;
	margin: 0px;
}
.headlineBoxContent {
	padding: 4px 0px 0px 1px;
}
.headlineBoxContent p {
	font-size: 0.9em;
	margin-top: 0px;
	margin-bottom: 14px;
}
img.left {
	float: left;
	margin: 0px 16px 16px 0px;
	padding: 0px;
}
.illustration {
	float: right;
	clear: right;
	margin: 0px 0px 1em 1em;
	padding: 1em;
	border: 1px solid #ccc;	
	margin-bottom: 1em;
	background: #fff;
}

.illustrationNoClear {
	float: right;
	margin: 0px 0px 1em 1em;
}

/* 
 *   TEMPLATE styles
 */

#title {
	background-image: url(/style/images/header/title_back.gif); 
	background-repeat: repeat-x;
	background-color: #7171a0;
}
#title {
/*	background-image: url(/style/xmas/snowflake2.gif); */
}
#search {
	padding: 17px;
	font-size: 0.9em;
	color: #FFFFFF;
	font-weight: bold;
	text-align: right;
}
#search .label {
	padding-right: 5px;
	vertical-align: middle;
}
#search input {
	font-size: 1.2em;
	border: 1px #2E2E61;
	vertical-align: middle;
}
#breadcrumbs {
	background-color: #EFEFEF;
	text-align: left;
}
#breadcrumbs ul {
	margin: 0px;
	padding: 5px 16px 6px 16px;
}
#breadcrumbs li {
	display: inline;
	font-size: 0.8em;
}
#breadcrumbs .current {
	font-weight: bold;
}
#breadcrumbs .login {
	margin: 0px;
	padding: 5px 16px 6px 16px;
	padding-right: 2em;
}
/* END of header styles */




/* START of footer styles */
#footer {
	text-align: center;
}
#footer ul {
	margin: 0px;
	padding: 0px 16px 16px 16px;
}
#footer li {
	display: inline;
	font-size: 0.8em;
}
/* END of footer styles */




/* 
 *     HEADLINE BOX
 */ 

.headlineBox {
	height: 157px;
	min-width: 745px;
	background: url(/style/images/headline/headline_back.gif) repeat-x;
	margin-bottom: 16px;
}
.headlineBoxLeft {
	width: 11px;
	height: 157px;
	float: left;
	background: url(/style/images/headline/headline_left.gif) left no-repeat;
}
.headlineBoxRight {
	width: 502px;
	height: 157px;
	float: right;
	background: url(/style/images/headline/wendy.jpg) right no-repeat;
}
.headlineBoxContent h3 {
	margin: 0px 0px 14px;
	color: #FFFFFF;
	font-size: 1em;
}
.headlineBoxContent h4 {
	font-size: 0.9em;
	margin: 0px;
}
.headlineBoxContent {
	float: left;
	width: 18em;
	padding: 4px 0px 0px 1px;
}
.headlineBoxContent a, .headlineBoxContent a:hover {
	color: #FFFFFF;
}
.headlineBoxContent p {
	color: #FFFFFF;
	font-size: 0.9em;
	margin-top: 0px;
	margin-bottom: 14px;
}



.boxContent ul.links.twocolumn {
	width: 49%;
	float: left;
	display: inline;
	padding: 0px;
}
.boxContent ul.links li {
	list-style-type: none; 
	/* list-style:  url(/style/images/doc_bullet.gif) ; */
	margin-bottom: 1em;
}

/*
 *    LINK LIST
 */

ul.linklist li {
	margin-bottom: 0.5em;
}

/*
 *    NEWS ITEMS 
 */



dl.news { 
	float: left;
	width: 47%;
	margin-right: 3%;
}

dl.news dt {
	font-weight: bold;
}
dl.news dd {
	margin-left: 0px;
	margin-bottom: 12px;
}

/* DEVEL CRAP */

img.thumbnail
{
	float: left;
	margin-right: 1em;
	margin-bottom: 0.5em;
}

img.body {
	float: left;
	margin-right: 1em;
	margin-bottom: 0.5em;
	border-color: black;
	border: 1px solid;
}


.welcome 
{
	font-size: 120%;
	font-weight: bold;
	margin-top: 1em;
	margin-bottom: 2em;
	text-align: justify;
}
.welcome .sig
{
	font-weight: normal;
}


.headline
{
	font-size: 140%;
}
.headline p
{
	text-align: justify;
	margin-top: 0.2em;
	margin-bottom: 1em;
}


.frontNewsItem
{
	width: 46%;
	float: left;
}
.frontNewsItem.odd
{
	clear: left;
}
.frontNewsItem.even
{
	margin-left: 4%;
}
.frontNewsItem p
{
	text-align: justify;
	margin-top: 0.2em;
	margin-bottom: 1em;
}
.frontNewsItem h3, .newsItem h3
{
	font-size: 110%;
}

.newsItem 
{
	width: 46%;
	float: left;
}
.newsItem.odd
{
	clear: left;
}
.newsItem.even
{
	margin-left: 4%;
}
.newsItem p
{
	margin-top: 0.2em;
	margin-bottom: 1em;
	text-align: justify;
}



.URIInfo
{
	font-size: 0.8em;
	background: #ffffc0;
	padding: 0.25em 1em 0.25em 1em;
	border: 1px solid #ccc;
	margin-bottom: 0;
	margin-top: 1em;
}

.intranetOnly
{
	background: #ccccff;
	padding: 1em;
	border: 1px solid #ccc;
	margin-bottom: 1em;
}

/*********************************************/
/* Timetables 
/*********************************************/
td.tableoutline {
	background-color: white;
	border-style: none; 
	border-width: 2px; 
	border-color: white white white white;
}	
td.tablewarning {
	background-color: #ffff00;
}
td.tableerror {
	background-color: #ff6060;
}
td.tabledull {
	background-color: #c0c0c0;
}

td.tablelight {
	background-color: #eee;
}	
/* New Browsers */
td.tabledark,td.tablelight,th.tableheading,td.tabledull,td.tableerror,td.tablewarning {
	border-style: solid; 
	border-width: 1px; 
	border-color: white white white white;
}
td.tabledark {
	background-color: white;
}	
th.tableheading {
	background-color: white;
}	

/** CHANNELS *******************************************/


.channel {
}
td.channelColMiddle, td.channelColFirst {
	padding-right: 0.5em;
	border-right: 1px dashed #ccc;
}
td.channelColMiddle, td.channelColLast {
	padding-left: 0.5em;
}

/*********************************************/

/** Lab bookings *************************************/

.bookingsSideBar
{
  position: absolute;
  left: 10px;
  width: 150px;
}

.bookingsBody
{
  margin-left: 170px;
  margin-right: 170px;
}

.bookingsTable
{
  display: table;
}

.bookingsTableRow
{
  display: table-row;
}

.bookingsTableHead
{
  width: 60px;
  height: 35px;
  display: table-cell;
  background-color: #DDDDDD;
  text-align: center;
}

.bookingsTableData
{
  width: 35px;
  height: 35px;
  display: table-cell;
  background-color: #DDDDDD;
  text-align: center;
}



/*****************************************************/

/*************************************************/
/* CHANGE COLOURS                                */
/*************************************************/

.boxHeader {
	background-color: #c39ebc;
}
.box {
	background-color: #e6cfe2;
}

#title {
        background-color: #8b4d52;
}
ul.ecsmenu li a span
{
	border-color: #4b2d2d;
	background-color: #a88;
	color: #fff;
}

ul.ecsmenu li a:hover span
{
	background-color: #8B4D4D;
}
#active a:link, #active a:visited, #active a:hover
{
	background-color: #8A4C51;
}
#navcontainer a:hover
{
	background-color: #8A4C51;
}

/*************************************************/
/* Anchors                                       */
/*************************************************/

a {
	/*color: #cA2C31; */
	color: #b01C21;
}
a:hover {
	color: #800000;
}
.intra_menu a {
	color: #000000;
}

/*************************************************/
/* LINKS                                         */
/*************************************************/


dl.links dt
{
	font-size: 120%;
	font-weight: bold;
}

dl.links dd
{
	margin-bottom: 0.5em;
}

/*************************************************/
/* Top Tabs                                      */
/*************************************************/




#tabs {
	white-space: nowrap;
    	width:100%;
    	background:white;
    	line-height:normal;
    	text-align: center;
}

#tabs ul {
    	border: 0;
    	margin:0;
    	padding:0;
	margin-top: 10px;
    	list-style:none;
	margin-bottom: 4px;
}
#tabs ul li {
	display: inline;
	margin: 0;
	border: 0;
}
#tabs a {
	text-decoration: none;
	color: #fff;
} 

/* Stuff for IE5-Mac, which gets overrided on all other browsers */
#tabs a
{
        background-color: #8b4d52;
	padding: 4px 4px 4px 4px;
}
#tabs ul li
{
	padding: 1px;
}	
/* end ie5-mac stuff */


/* Commented Backslash Hack hides rules from IE5-Mac \*/  
#tabs a
{
	padding: 0;
	background: none;
}
#tabs ul li
{
	padding: 12px 30px 4px 0px;	
    	background:url("/style/images/header/tab_right.png") no-repeat right top;
}
#tabs ul li.last 
{
    	background:url("/style/images/header/tab_right_end.png") no-repeat right top;
}
#tabs .first a
{
    	padding: 12px 0px 4px 37px;
    	background:url("/style/images/header/tab_left_end.png") no-repeat left top; 
}

                                                         
/* End IE5-Mac hack */

/* MS Search Server default styling for ECS intranet */
div.mssearch_main_search_box {
    padding: 1em;
    text-align: center;
    font-size: 1.2em;
}

div.mssearch_header_bar {
    background-color: #EEEEEE;
    padding: 0.4em;
    padding-right: 0.8em;
    margin-top: 2em;
    margin-bottom: 1em;
    border-top: 1px solid black;
    text-align: right;
}
span.mssearch_scope {
    float: left;
}

div.mssearch_result {
    padding-bottom: 1em;
}

a.mssearch_result_title {
    font-size: 1.2em;
}

span.mssearch_result_url {
    color: green;
}

span.mssearch_result_highlight {
    font-weight: bold;
}
div.mssearch_navigation {
    text-align: center;
    padding-top: 1em;
}

div.mssearch_navigation .mssearch_current_bucket {
    font-weight: bold;
    padding: 0.3em;
}

div.mssearch_navigation .mssearch_bucket a {
    padding: 0.2em;
}
a.mssearch_navigation_prev {
    font-weight: bold;
    padding-right: 0.5em;
}

a.mssearch_navigation_next {
    font-weight: bold;
    padding-left: 0.5em;
}

input#mssearch_query_field {
    width: 25em;
}

/* hide tabs on the syllabus page */
.ecs-syllabus .uos_tabBar {
	display: none;
}

