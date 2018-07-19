var Studio = function(){
    var self = this;
    
    self.log = function(){
        if(console) console.log.apply(null, arguments);
    }

    self.find = function(thing, array){
        return 0 <= array.indexOf(thing);
    };

    self.remove = function(thing, array){
        var result = [];
        for(var el of array){
            if(el != thing) result.push(el);
        }
        return result;
    };

    var fileObjects = [];
    self.filePayload = function(root){
        root = root || document;
        var result = [];
        [].forEach.call(root.querySelectorAll(".image"), function(image){
            if(!image.classList.contains("removed")){
                if(image.dataset.file){
                    result.push(fileObjects[parseInt(image.dataset.file)]);
                }else if(image.dataset.id){
                    result.push(image.dataset.id);
                }else{
                    log("Warning: image without id or file", image);
                }
            }
        });
        return result;
    };
    
    var initUpload = function(root){
        self.log("Init upload", root);
        
        var fileSelect = root.querySelector(".new-image [type=file]");
        var images = root.querySelector(".images");
        
        var registerImage = function(image){
            image.querySelector(".remove").addEventListener("click", function(){
                if(image.classList.contains("removed")){
                    image.classList.remove("removed");
                    image.querySelector(".remove i").classList.remove("fa-undo");
                    image.querySelector(".remove i").classList.add("fa-trash-alt");
                }else{
                    image.classList.add("removed");
                    image.querySelector(".remove i").classList.add("fa-undo");
                    image.querySelector(".remove i").classList.remove("fa-trash-alt");
                }
                if(images.querySelectorAll(".image:not(.removed)").length == 0){
                    fileSelect.setAttribute("required", "required");
                }else{
                    fileSelect.removeAttribute("required");
                }
            });

            image.addEventListener("dragstart", function(ev){
                ev.dataTransfer.effectAllowed = "move";
                image.classList.add("move");
            });

            image.addEventListener("dragover", function(ev){
                ev.preventDefault();
                ev.dataTransfer.dropEffect = "move";
                var source = images.querySelector(".image.move");
                var target = ev.target;
                while(target && !target.classList.contains("image")) target = target.parentNode;
                if(source && target && target != source){
                    var g = target.getBoundingClientRect();
                    if(ev.clientY < g.top+(g.bottom-g.top)/2){
                        images.insertBefore(source, target);
                    }else{
                        images.insertBefore(source, target.nextSibling);
                    }
                }
            });

            image.addEventListener("dragend", function(ev){
                ev.preventDefault();
                image.classList.remove("move");
            });
        };
        
        var showFile = function(file){
            var image = document.createElement("div");
            var img = document.createElement("img");
            var remove = document.createElement("label");

            // Start loading image as soon as possible
            var reader = new FileReader();
            reader.onload = function(){ img.src = reader.result; };
            reader.readAsDataURL(file);

            // Prepare the rest.
            image.dataset.file = fileObjects.length;
            fileObjects.push(file);
            image.classList.add("image");
            remove.classList.add("remove");
            remove.innerHTML = '<i class="fas fa-fw fa-trash-alt"></i>';
            image.appendChild(img);
            image.appendChild(remove);
            registerImage(image);
            images.insertBefore(image, images.querySelector(".new-image"));
        };

        fileSelect.addEventListener("change", function(){
            fileSelect.removeAttribute("required");
            for(var i=0; i<fileSelect.files.length; i++){
                showFile(fileSelect.files[i]);
            }
            fileSelect.value = '';
        });

        [].forEach.call(images.querySelectorAll(".image"), registerImage);

        root.querySelector("[type=submit]").addEventListener("click", function(ev){
            // Gather form data
            var form = new FormData();
            form.append("data-format", "json");
            form.append("title", root.querySelector("[name=title]").value);
            form.append("description", root.querySelector("[name=description]").value);
            form.append("tags", root.querySelector("[name=tags]").value);
            form.append("visibility", root.querySelector("[name=visibility]").value);
            if(root.querySelector("[name=upload]")){
                form.append("upload", root.querySelector("[name=upload]").value);
            }
            self.filePayload(images).forEach(function(file){
                form.append("file[]", file);
            });
            // Submit form via AJAX
            var request = new XMLHttpRequest();
            request.responseType = 'json';
            request.onload = function(ev){
                self.log("Submission complete", request);
                if(request.status == 200){
                    window.location = request.response["data"]["url"];
                }else{
                    document.querySelector("#error").innerHTML = request.response["message"];
                }
            };
            request.open("POST", ev.target.getAttribute("formaction"));
            self.log("Submitting", form);
            request.send(form);
            ev.preventDefault();
            return false;
        });
    };

    var initGallery = function(root){
        self.log("Init gallery", root);
    };

    var initView = function(root){
        self.log("Init view", root);

        [].forEach.call(root.querySelectorAll(".image img"), function(img){
            img.addEventListener("click", function(){
                if(img.classList.contains("full")){
                    img.classList.remove("full");
                }else{
                    img.classList.add("full");
                }
            });
        });
    };

    self.init = function(root){
        var upload = root.querySelector(".upload");
        var gallery = root.querySelector(".gallery");
        var view = root.querySelector(".view");
        
        if(upload) initUpload(upload);
        if(gallery) initGallery(gallery);
        if(view) initView(view);
    };
};

var studio = null;
document.addEventListener("DOMContentLoaded", function(){
    studio = new Studio();
    studio.init(document);
});
