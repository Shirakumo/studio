document.addEventListener("DOMContentLoaded", function(){
    var log = function(){
        if(console) console.log.apply(null, arguments);
    }
    
    var initUpload = function(root){
        log("Init upload", root);

        var idCounter = 0;
        var toUpload = {};
        var toDelete = [];

        var registerNewImage = null;
        var showFile = function(root, file){
            var image = document.createElement("img");
            var remove = document.createElement("i");
            if(file){
                var reader = new FileReader();
                root.dataset.tid = idCounter++;
                toUpload[root.dataset.tid] = file;
                reader.onload = function(){ image.src = reader.result; };
                reader.readAsDataURL(file);
            }
            // FIXME: D&D
            remove.classList.add("remove", "fas", "fa-fw", "fa-trash-alt");
            remove.addEventListener("click", function(){
                if(root.dataset.tid){
                    delete toUpload[root.dataset.tid];
                } else {
                    toDelete.push(root.dataset.id);
                }
                root.removeChild(image);
                root.removeChild(remove);
            });
            root.appendChild(image);
            root.appendChild(remove);
        };
        
        var addNewImage = function(old){
            log("New image", old);
            // Do this first so subsequent files are not required.
            var file = old.querySelector("[type=file]");
            file.removeAttribute("required");
            // Clone and register a copy of the selector.
            var newImage = old.cloneNode(true);
            newImage.querySelector("[type=file]").value = "";
            root.querySelector(".images").appendChild(newImage);
            registerNewImage(newImage);
            // Turn the old selector into an image preview.
            file.removeAttribute("id");
            old.classList.remove("new-image");
            old.classList.add("image");
            var label = old.querySelector("label");
            label.parentNode.removeChild(label);
            // Load the image files.
            for(var i=0; i<file.files.length; i++){
                showFile(old, file.files[i]);
            }
        };
        
        registerNewImage = function(old){
            old.querySelector("[type=file]").addEventListener("change", function(){
                addNewImage(old);
            });
        };
        
        registerNewImage(root.querySelector(".new-image"));

        root.querySelector("[type=submit]").addEventListener("click", function(ev){
            var form = new FormData();
            form.append("title", root.querySelector("[name=title]").value);
            form.append("description", root.querySelector("[name=description]").value);
            form.append("tags", root.querySelector("[name=tags]").value);
            form.append("data-format", "json");
            for(var i in toUpload){
                form.append("file[]", toUpload[i]);
            }
            for(var i in toDelete){
                form.append("delete[]", toDelete[i]);
            }
            var request = new XMLHttpRequest();
            request.responseType = 'json';
            request.onload = function(ev){
                log("Submission complete", request);
                if(request.status == 200){
                    window.location = request.response["data"]["url"];
                }else{
                    document.querySelector("#error").innerHTML = request.response["message"];
                }
            };
            request.open("POST", root.getAttribute("action"));
            log("Submitting", form);
            request.send(form);
            ev.preventDefault();
            return false;
        });
    };

    var initGallery = function(root){
        log("Init gallery", root);
    };

    var initView = function(root){
        log("Init view", root);
    };

    var init = function(root){
        var upload = root.querySelector(".upload");
        var gallery = root.querySelector(".gallery");
        var view = root.querySelector(".view");
        
        if(upload) initUpload(upload);
        if(gallery) initGallery(gallery);
        if(view) initView(view);
    };

    init(document);
});
