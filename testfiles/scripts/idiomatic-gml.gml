// donated by Sidorakh: https://twitter.com/sidorakh

function gamepad_check_button_pressed_multiple(device,buttons) {
	for (var i=0;i<array_length(buttons);i++) {
		if (gamepad_button_check_pressed(device,buttons[i])) return true;	
	}
	return false;
}

function gamepad_check_button_multiple(device,buttons) {
	for (var i=0;i<array_length(buttons);i++) {
		if (gamepad_button_check(device,buttons[i])) return true;	
	}
	return false;
}

function gamepad_check_button_released_multiple(device,buttons) {
	for (var i=0;i<array_length(buttons);i++) {
		if (gamepad_button_check_released(device,buttons[i])) return true;	
	}
	return false;
}

function keyboard_check_multiple(keys) {
	for (var i=0;i<array_length(keys);i++) {
		if (keyboard_check(keys[i])) {
			return true;	
		}
	}
	return false;
}

function keyboard_check_pressed_multiple(keys) {
	for (var i=0;i<array_length(keys);i++) {
		if (keyboard_check_pressed(keys[i])) {
			return true;	
		}
	}
	return false;
}

function keyboard_check_released_multiple(keys) {
	for (var i=0;i<array_length(keys);i++) {
		if (keyboard_check_released(keys[i])) {
			return true;	
		}
	}
	return false;
}

function input_check_pressed(key) {
	if (obj_input.pressed[$ key]) {
		return true;
	}
	for (var i=0;i<gamepad_get_device_count();i++) {
		if (array_length(obj_input.controller_values)-1 < i) continue;
		if (gamepad_is_connected(i)	) {
			if (obj_input.controller_values[i].state.pressed[$ key]) {
				return true;
			}
		}
	}
	return false;
}

function input_check(key) {
	if (obj_input.held[$ key]) {
		return true;
	}
	for (var i=0;i<gamepad_get_device_count();i++) {
		if (array_length(obj_input.controller_values)-1 < i) continue;
		if (gamepad_is_connected(i)	) {
			if (obj_input.controller_values[i].state.held[$ key]) {
				if (key == "left" || key == "right") {
					if (abs(obj_input.controller_values[i].axes.horizontal) > obj_input.deadzone) {
						return abs(obj_input.controller_values[i].axes.horizontal);
					}
				}
				if (key == "up" || key == "down") {
					if (abs(obj_input.controller_values[i].axes.vertical) > obj_input.deadzone) {
						return abs(obj_input.controller_values[i].axes.vertical);
					}
				}
				return true;
			}
		}
	}
	return false;
}

function input_check_released(key) {
	if (obj_input.released[$ key]) {
		return true;
	}
	for (var i=0;i<gamepad_get_device_count();i++) {
		if (array_length(obj_input.controller_values)-1 < i) continue;
		if (gamepad_is_connected(i)	) {
			if (obj_input.controller_values[i].state.released[$ key]) {
				return true;
			}
		}
	}
	return false;
}