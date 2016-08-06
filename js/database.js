function addLesson()
{
    var branch = document.getElementById('branch').value;
    var title = document.getElementById('title').value;
    var gdriveid = document.getElementById('gdriveid').value;
    document.getElementById('createLessonKey').innerHTML = 'Lesson created! Key : "' + writeNewLesson(branch, title, gdriveid) + '"';
}

function writeNewLesson(branch, title, gDriveId) {
  var lessonData = {
    title: title,
    gdriveid: gDriveId
  };

  var newLessonKey = firebase.database().ref().child('lessons').child(branch).push().key;

  var updates = {};
  updates['/lessons/' + branch + '/' + newLessonKey] = lessonData;

  firebase.database().ref().update(updates);

  return newLessonKey;
}

function getLesson(branch, key, callback)
{
    firebase.database().ref('lessons/' + branch + '/' + key).once('value').then(function(snapshot) {
        var lessonData = {
          title: snapshot.val().title,
          gdriveid: snapshot.val().gdriveid
        };

        //Do stuff with lesson data
        callback(lessonData);
    });
}

function getLessonState(callback)
{
    if(uid == null)
    {
        alert('uid not set!');
        return;
    }

    var branch = $('#google-doc-iframe').attr('subject');
    var key = $('#google-doc-iframe').attr('key');

    firebase.database().ref('user-progress/' + uid + '/' + branch + '/' + key).once('value').then(function(snapshot) {
        var lessonData = {
            progress: snapshot.val().progress
        };

        //Do stuff with lesson data
        callback(lessonData);
    });
}

function setLessonState(state)
{
    if(uid == null)
    {
        alert('uid not set!');
        return;
    }

    var branch = $('#google-doc-iframe').attr('subject');
    var key = $('#google-doc-iframe').attr('key');

    var lessonData = {
        progress: state
    };

    var updates = {};
    updates['user-progress/' + uid + '/' + branch + '/' + key] = lessonData;

    return firebase.database().ref().update(updates);
}
