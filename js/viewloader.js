window.onhashchange = function() {
    loadDashboard();
}

function loadView(toLoad, id, callback) {
    if (!firebase.auth().currentUser) {
        if (!(toLoad == 'views/register.html' || toLoad == 'views/login.html' || toLoad == 'views/createLesson'))
            toLoad = 'views/home.html';
    }

    if (toLoad == 'views/createLesson')
        if (firebase.auth().currentUser.uid != '6239c43a-7ac6-45c7-a828-a7a09e82df67')
            return;

    if (toLoad == 'views/register.html' || toLoad == 'views/login.html' || toLoad == 'views/home.html')
        window.location.hash = '';

    if (id == null)
        id = 'content';

    var xhttp = new XMLHttpRequest();
    xhttp.onreadystatechange = function() {
        if (xhttp.readyState == 4 && xhttp.status == 200) {
            document.getElementById(id).innerHTML = xhttp.responseText;
            if (toLoad == 'views/home.html')
                loadSidebar();
            if (callback != null)
                callback();
        }
    };
    xhttp.open('GET', toLoad, true);
    xhttp.send();
}

function loadDashboard(subject) {
    if (!firebase.auth().currentUser) //Go to login page if not signed in
    {
        loadView('views/home.html');
        return;
    }
    if (subject == null) //If no kmap set, load one in url hash
    {
        subject = window.location.hash.replace('#', '');
        if (subject == null || subject == '') //If still no kmap, load home page
        {
            loadView('views/home.html');
            return;
        } else if (String(subject).indexOf('/') != -1) //If it is not a kmap... ooops, load the lesson page
        {
            var parts = String(subject).split('/');
            if (parts[1] == null) //Nevermind, some idiot put a '/' behind the kmap link
            {} else {
                getLesson(parts[0], parts[1], function(data) {
                    if (data == null) {
                        alert("This lesson does not exist!");
                    }
                    loadGDriveView(parts[0], data.gdriveid, data.title, parts[1]);
                });
                return;
            }
        }
    }

    var xhttp = new XMLHttpRequest();
    xhttp.onreadystatechange = function() {
        if (xhttp.readyState == 4 && xhttp.status == 200) {
            window.location.hash = subject;
            document.getElementById('content').innerHTML = xhttp.responseText;
            initKmap(subject);
        }
    };
    xhttp.open('GET', 'views/dashboard.html', true);
    xhttp.send();
}

function loadSidebar() {
    updateContent();
    if (user) {
        loadView('views/welcome.html', 'sidebar', function() {
            document.getElementById('welcomemessage').innerHTML = 'Welcome back, ' + email + '!';
        });
    } else {
        loadView('views/login.html', 'sidebar');
    }
}

function loadGDriveView(subject, gDriveId, title, key) {
    var xhttp = new XMLHttpRequest();
    xhttp.onreadystatechange = function() {
        if (xhttp.readyState == 4 && xhttp.status == 200) {
            window.location.hash = subject + '/' + key;
            document.getElementById('content').innerHTML = xhttp.responseText;
            document.getElementById('lesson-title').innerHTML = title;

            $.get('https://docs.google.com/document/d/' + gDriveId + '/pub?embedded=true', function(html) {
                $('#google-doc-iframe').attr('srcdoc', html);
                $('#google-doc-iframe').attr('subject', subject);
                $('#google-doc-iframe').attr('key', key);
                updateLessonState();
                setTimeout(function() {
                    document.getElementById('google-doc-iframe').style.height = document.getElementById('google-doc-iframe').contentWindow.document.body.scrollHeight + 'px';
                    $('#google-doc-iframe').contents().find('a[href^="http://"]').attr('target', '_blank');
                    $('#google-doc-iframe').contents().find('a[href^="https://"]').attr('target', '_blank');
                }, 1000);
            });
        }
    };
    xhttp.open('GET', 'views/lesson.html', true);
    xhttp.send();
}

function updateLessonState() {
    var state;
    getLessonState(function(data) {
        state = data.progress;
        document.getElementById('lesson-progress').innerHTML = state;
    });
}

function initKmap(subject) {
    var xhttp = new XMLHttpRequest();
    xhttp.onreadystatechange = function() {
        if (xhttp.readyState == 4 && xhttp.status == 200) {
            document.getElementById('kmap').innerHTML = xhttp.responseText;
            $('#kmap').css('background-color', '#fefbf7');
            $($('#kmap').children()[0]).css('width', '100%');
            $($('#kmap').children()[0]).css('height', '750px');
            svgPanZoom($('#kmap').children()[0], {
                zoomScaleSensitivity: 0.3,
                dblClickZoomEnabled: false
            });

            //Set classes for each svg link
            var $elements = $('a', 'svg');
            $elements.each(function(index, e1) {
                $(e1).children().each(function(index, e2) {
                    $(e1).attr('clickJs', $(e1).attr('xlink:href'));
                    $(e1).css('cursor', 'pointer');
                    $(e2).addClass('kmap' + $(e1).attr('xlink:href'));
                    $(e1).click(
                        function(event) {
                            getLesson(subject, $(e1).attr('clickJs'), function(data) {
                                loadGDriveView(subject, data.gdriveid, data.title, $(e1).attr('clickJs'));
                            });
                        });
                    $(e1).removeAttr('xlink:href');
                });
            });
            updateUserKmapProgress(subject);
        } else if (xhttp.readyState == 4) {
            document.getElementById('loading').innerHTML = 'Failed to load kmap \'' + subject + '\'. Error: ' + xhttp.status;
        }
    };
    xhttp.open('GET', 'img/kmap-' + subject + '.svg', true);
    xhttp.send();
}

function updateUserKmapProgress(branch) {
    firebase.database().ref('user-progress/' + uid + '/' + branch).once('value').then(function(snapshot) {
        snapshot.forEach(function(item, index) {
            console.log(item.key);
            console.log(item.val().progress);
            if (document.getElementsByClassName('kmap' + item.key) != null) {
                switch (item.val().progress) {
                    case 'complete':
                        $('.kmap' + item.key).attr('fill', '#00ff00')
                        break;
                    case 'inprogress':
                        $('.kmap' + item.key).attr('fill', '#ffff00')
                        break;
                    default:
                        $('.kmap' + item.key).attr('fill', '#ffffff')
                        break;
                }
            }
        });
    });
}
