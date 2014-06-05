<?php header('Content-Type: text/html; charset=utf-8'); ?>
<html>
    <head>
        <title>Uopfordret ansøgning</title>

        <link rel="stylesheet" type="text/css" href="add.css">
        <link rel="stylesheet" media="all" href="review.css" />
        <script src="//code.jquery.com/jquery-1.10.2.js"></script>
        <script src="//code.jquery.com/ui/1.10.4/jquery-ui.js"></script>
        <link rel="stylesheet" href="//code.jquery.com/ui/1.10.4/themes/smoothness/jquery-ui.css" />
        <style type="text/css" >
            .note{
                width:100%;
            }
        </style>

    </head>

    <body style="margin-left: 20px;width:66%;">

        <h1 style="position: relative; left: 25%"> Udfyld felterne nedenfor </h1>
        <img src="imgs/logo.png" alt="" style="position: absolute; right: -25%; top: 10px;"/>

        <div class="mainContainer">
            <form id="application" style="position: relative;left:25%;" action="insert.php" method="GET">

                <div class="box" style="float: left; margin-right: 25%">

                    <h1 class="header">Info og kompetencer</h1>
                    <label>Navn</label>
                    <p class="note">
                        <input class="span3"  type="text" name="name"/>
                    </p>
                    <label>Adresse</label>
                    <p class="note">
                        <input class="span3"  type="text" name="address"/>
                    </p>
                    <label>Postnummer</label>
                    <p class="note">
                        <input class="span3" type="text" name="postal"/>
                    </p>
                    <label>By</label>
                    <p class="note">
                        <input class="span3"  type="text" name="city"/>
                    </p>
                    <label>Telefonnummer</label>
                    <p class="note">
                        <input class="span3"  type="text" name="telephone"/>
                    </p>
                    <label>E-mail</label>
                    <p class="note">
                        <input class="span3"  type="text" name="email"/>
                    </p>
                    <label>Kommune</label>
                    <p class="note">
                        <input class="span3"  type="text" name="munipicial"/>
                    </p>
                    <label>Ryger du på arbejdet?</label>
                    <p class="note">
                        <input type="checkbox" value='1'  name="smoker"/>
                    </p>
                    <label>En kort beskrivelse af dig som person</label>
                    <p class="note">
                        <textarea class="span3" type="text" name="personaldescription" ></textarea>
                    </p>
                    <label>Hvorfor skal vi ansætte dig?</label>
                    <p class="note">
                        <textarea class="span3" type="text" name="hirereason"></textarea>
                    </p>
                </div>

                <div class="box" style="float: left;">
                    <h1 class="header">Erfaringer</h1>

                    <label>Tidligere uddannelse</label>
                    <p class="note">
                        <textarea name="school"></textarea>
                    </p>

                    <label>Tidligere beskæftigelse</label>
                    <p class="note">
                        <textarea name="earlierjobs"></textarea>
                    </p>

                    <label>Har du truckcertifikat?</label>
                    <p class="note">
                        <input class="span3"  value='1' type="checkbox" name="truckcert"/>
                    </p>
                    <label>Har du hygiejnebevis?</label>
                    <p class="note">
                        <input class="span3"  value='1' type="checkbox" name="cleancert"/>
                    </p>
                    <label>Har du erfaring med SAP?</label>
                    <p class="note">
                        <input class="span3"  value='1' type="checkbox" name="knowasp"/>
                    </p>
                    <label>Har du erfaring med LEAN?</label>
                    <p class="note">
                        <input class="span3"  value='1' type="checkbox" name="knowlean"/>
                    </p>
                    <label>Kan du tale og forstå engelsk?</label>
                    <p class="note">
                        <input class="span3"  value='1' type="checkbox" name="knowenglish"/>
                    </p>
                    <label>Kan du læse engelsk?</label>
                    <p class="note">
                        <input class="span3"  value='1' type="checkbox" name="knowenglishtext"/>
                    </p>
                </div>



                <div class="box" style="float: left;margin-right: 25%;">
                    <!--                <fieldset>
                                        <legend>Personlige oplysninger</legend>-->
                    <h1 class="header"> Arbejdsmuligheder</h1>
                    <label>Hvornår kan du tidligst tiltræde?</label>
                    <p class="note">
                        <input style="height: 50px;top: -00px" id="datepicker" name="readydate"/>
                    </p>
                    <p style="color:black; font-size: 14pt;">
                        Hvad former for arbejdstider kan du? (ferie, weekend, hverdage)
                    </p> 
                    <label>Fuldtidsarbejde</label>
                    <p class="note">
                        <input class="span3"  value='1' type="checkbox" name="usualdays"/>
                    </p>
                    <label>Ferieafløser</label>
                    <p class="note">
                        <input class="span3"  value='1' type="checkbox" name="vacationdays"/>
                    </p>
                    <label>Weekendafløser</label>
                    <p class="note">
                        <input class="span3"  value='1' type="checkbox" name="weekendays"/>
                    </p>

                </div>


                <div class="box" style="float: left;margin-right: 5%;">

                    <h1 class="header"> Tidligere ansat</h1>

                    <label>Har du arbejdet hos arla før ?</label>
                    <p class="note">
                        <input class="span3" value='1' type="checkbox" name="prevarla"/>
                    </p>

                    <label>Hvis ja, hvor?</label>
                    <p class="note">
                        <input class="span3"  type="text" name="prevarlawhere"/>
                    </p>
                    <label>Og med hvad?</label>
                    <p class="note">
                        <input class="span3"  type="text" name="prevarlawhat"/>
                    </p>

                </div>



                <div class="box" style="float:left;width:50%;" >
                    <hr>
                    <h3> Ved at klikke Send, giver du din samtrykke til, at disse oplysninger kan opbevares i 12 måneder fra dags dato med henblik på evt. fremtidige ansættelse.</h3>
                    <input class="btn btn-primary arlabtn" style="border:green 1px ridge;" type="submit" value="Send ansøgning"/>
                </div>
            </form>
        </div>

        <script>
            $("#datepicker").datepicker();

            var $inputs = $('#application :input');
            $inputs.each(function() {
                if (this.localName == "input") {
                    $(this).keypress(function(e) {
                        if (e.which === 13) {
                            return false;
                        }
                    });
                }
            });
        </script>
    </body>
</html> 
