package dk.n144.pervcomp;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.widget.EditText;

public class IdentifyActivity extends Activity {

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_identify);
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		// Inflate the menu; this adds items to the action bar if it is present.
		getMenuInflater().inflate(R.menu.identify, menu);
		return true;
	}

	public void signIn(View v) {
	    Intent intent = new Intent(this, MapActivity.class);
	    EditText editText = (EditText) findViewById(R.id.name_textfield);
	    String message = editText.getText().toString();
	    intent.putExtra("user_name", message);
	    
	    // Throws an error because of spaces in the username.
	    if (message.contains(" ")) {
	    	editText.setError("Please don't enter spaces");
	    }
	    else {
	    	startActivity(intent);
	    }
	}
}
