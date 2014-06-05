<!DOCTYPE html>
<html>
    <head>
        <link rel="stylesheet" type="text/css" href="review.css">
        <title></title>

    </head>
    <body>
        <div style="display:inline-block;width:100%;">
            <div>
                <h1 class="header" style="display: inline-block;width:100%">Søgning</h1>
                <img style="display: inline-block; right: 0px; position: absolute; margin-right: -10px;top:-22px" src="imgs/logo.png" />
            </div>
            <hr>
            <form method="GET" action="review.php">
                <div class="reviews">
                    <div class="line">
                        <p>SAP</p><input type="checkbox"  name="ssap" <?php mc($ssap) ?> value="1"/>
                        <p>Truck certifikat</p><input type="checkbox" <?php mc($truck) ?> name="truck"  value="1"/>
                        <p>Hygiejne bevis</p><input type="checkbox" <?php mc($hyg) ?> name="hyg"  value="1"/>
                        <p>Weekend </p><input type="checkbox" <?php mc($weekend) ?> name="weekend" value="1"/>
                        <p>Hverdage</p><input type="checkbox" <?php mc($everyday) ?> name="everyday"  value="1"/>
                    </div>
                    <div class="line">
                        <p>Ferie</p><input type="checkbox" <?php mc($vacation) ?> name="vacation" value="1"/>
                        <p>LEAN</p><input type="checkbox" <?php mc($lean) ?> name="lean"  value="1"/>
                        <p>Dag</p><input type="checkbox" <?php mc($day) ?> name="day"  value="1"/>
                        <p>Nat</p><input type="checkbox" <?php mc($night) ?> name="night"  value="1"/> 
                        <p>Faglært</p><input type="checkbox" <?php mc($job) ?> name="job"  value="1"/> 
                        <p>Ryger på arbejdet</p><input type="checkbox" <?php mc($smoke) ?> name="smoke"  value="1"/>
                        <p>Kan engelsk</p><input type="checkbox" name="eng" <?php mc($eng) ?> value="1"/>
                    </div>

                    <div class="line">
                        <p>Arbejdet hos arla før?</p><input type="checkbox" <?php mc($prev) ?> name="prevarla"  value="1"/>
                        <p>Nærmeste tiltrædningsdato</p><input type="date" name="date" value="<?php dt($date) ?>"/>
                        <p>Fritekst søgning</p><input type="text" name="freetext" value="<?php dt($freetext) ?>"/>
                    </div>

                </div>
                <input style="width: 30%" class="arlabtn" type="submit" value="søg"/>
            </form>
            <form action="review.php?">
                <input type="submit" class="arlabtn"  value="nulstil søging"/>
            </form>
            <hr> 

